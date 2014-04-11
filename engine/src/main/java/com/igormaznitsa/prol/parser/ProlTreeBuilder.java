/* 
 * Copyright 2014 Igor Maznitsa (http://www.igormaznitsa.com).
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.igormaznitsa.prol.parser;

import com.igormaznitsa.prol.containers.KnowledgeBase;
import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.TermList;
import com.igormaznitsa.prol.data.Operator;
import com.igormaznitsa.prol.containers.OperatorContainer;
import com.igormaznitsa.prol.data.NumericTerm;
import com.igormaznitsa.prol.data.TermStruct;
import com.igormaznitsa.prol.data.Var;
import com.igormaznitsa.prol.exceptions.ProlCriticalError;
import com.igormaznitsa.prol.exceptions.ParserException;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.parser.ProlTokenizer.ProlTokenizerResult;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

/**
 * The class implements the prol tree builder. It's very important class because
 * it allows to make term tree from read terms.
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 * @see com.igormaznitsa.prol.parser.ProlTokenizer
 */
public final class ProlTreeBuilder {

  /**
   * Inside array of operators which will be used to read a prolog phrase
   */
  private final OperatorContainer[] OPERATORS_PHRASE;

  /**
   * Inside array of operators which will be used to read an inside list
   */
  private final OperatorContainer[] OPERATORS_INSIDE_LIST;

  /**
   * Inside array of operators which will be used to find the end of a list
   */
  private final OperatorContainer[] OPERATORS_END_LIST;

  /**
   * Inside array of operators which will be used to read a structure
   */
  private final OperatorContainer[] OPERATORS_INSIDE_STRUCT;

  /**
   * Inside array of operators which will be used to read a sub-block inside of
   * a block
   */
  private final OperatorContainer[] OPERATORS_SUBBLOCK;

  /**
   * Inside auxulary class represents a tree item
   *
   * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
   */
  private static final class TreeItem {

    /**
     * The tree builder owner of the item
     */
    private final ProlTreeBuilder builder;

    /**
     * The term saved by the item
     */
    private final Term savedterm;
    /**
     * The left branch of the tree item
     */
    private TreeItem leftBranch;

    /**
     * The right branch of the item
     */
    private TreeItem rightBranch;

    /**
     * The link to the owner of the item
     */
    private TreeItem owner;

    /**
     * The string position of the item at the read stream
     */
    private final int strPos;

    /**
     * The line position of the item at the read stream
     */
    private final int lineNum;

    /**
     * The term has been placed into brakes
     */
    private final boolean atBrakes;

    /**
     * The constructor
     *
     * @param builder the builder which has the item, must not be null
     * @param term the term read from the input stream, must not be null
     * @param atBrakes the flag shows that the term was in the brakes so it has
     * the max priority
     * @param lineNum the line number of the line where the term has been found
     * @param strPos the string position of the read stream at the input stream
     */
    private TreeItem(final ProlTreeBuilder builder, final Term term, final boolean atBrakes, final int lineNum, final int strPos) {
      savedterm = term;
      this.builder = builder;
      this.strPos = strPos;
      this.lineNum = lineNum;
      this.atBrakes = atBrakes;
    }

    /**
     * Get the priority oa the term.
     *
     * @return the priority of the term
     */
    private final int getPriority() {
      if (atBrakes) {
        return 0;
      }
      return savedterm.getPriority();
    }

    /**
     * Set the right branch
     *
     * @param item the right branch for the term
     */
    private final void setRightBranch(final TreeItem item) {
      rightBranch = item;
      if (item != null) {
        item.owner = this;
      }
    }

    /**
     * Make an other ietm as the right branch
     *
     * @param item the item which will be used as the right branch
     * @return the item which will be used as the root, it can be this item or
     * setted item (it depends on priorities)
     */
    private final TreeItem makeAsRightBranch(final TreeItem item) {
      TreeItem currentSubbranch = rightBranch;
      setRightBranch(item);
      item.setLeftBranch(currentSubbranch);
      if (item.getItemType() == Term.TYPE_OPERATOR) {
        return item.getPriority() == 0 ? this : item;
      }
      return this;
    }

    /**
     * Make an other ietm as the left branch
     *
     * @param item the item which will be used as the left branch
     * @return the item which will be used as the root, it can be this item or
     * setted item (it depends on priorities)
     */
    private final TreeItem makeAsOwnerWithLeftBranch(final TreeItem item) {
      this.replaceForOwner(item);
      item.setLeftBranch(this);
      return item;
    }

    /**
     * Get the right branch of the item, can be null
     *
     * @return the right branch or null
     */
    private final TreeItem getRightBranch() {
      return rightBranch;
    }

    /**
     * Set the left branch of the item
     *
     * @param item the left branch for the item
     */
    private final void setLeftBranch(final TreeItem item) {
      leftBranch = item;
      if (item != null) {
        item.owner = this;
      }
    }

    /**
     * Get the left branch of the item, can be null
     *
     * @return the left branch of the item or null
     */
    private final TreeItem getLeftBranch() {
      return leftBranch;
    }

    /**
     * Get the type of the saved term by the item
     *
     * @return
     */
    private final int getItemType() {
      return savedterm.getTermType();
    }

    /**
     * Find the root of the tree where the item has been placed
     *
     * @return the root item for the tree where the item has been placed
     */
    private final TreeItem findRoot() {
      if (owner == null) {
        return this;
      }
      return owner.findRoot();
    }

    /**
     * Find the first node from owners of the item which has the same or lower
     * priority
     *
     * @param priority the priority to find an item
     * @return found root itemn which has the equal or less priority than the
     * value.
     */
    private final TreeItem findFirstNodeWithSuchOrLowerPriority(final int priority) {
      TreeItem result = null;
      if (getPriority() >= priority || owner == null) {
        result = this;
      }
      else {
        result = owner.findFirstNodeWithSuchOrLowerPriority(priority);
      }
      return result;
    }

    /**
     * Replace the owner by other item
     *
     * @param newItem the item to replace current owner
     */
    private final void replaceForOwner(final TreeItem newItem) {
      if (owner == null) {
        newItem.owner = null;
        return;
      }

      if (this.equals(owner.getLeftBranch())) {
        owner.setLeftBranch(newItem);
      }
      else {
        owner.setRightBranch(newItem);
      }
    }

    /**
     * Get the operator type, the saved term must be an operator
     *
     * @return the operator type as integer
     */
    private final int getOperatorType() {
      return ((Operator) savedterm).getOperatorType();
    }

    /**
     * Validate the tree item for its saved term
     *
     * @return true if the tree item is valid and false if it's not
     */
    private final boolean validate() {
      if (savedterm.getTermType() == Term.TYPE_OPERATOR) {
        final int priority = getPriority();

        switch (((Operator) savedterm).getOperatorType()) {
          case Operator.OPTYPE_FX: {
            return leftBranch == null && (rightBranch != null && rightBranch.getPriority() < priority);
          }
          case Operator.OPTYPE_FY: {
            return leftBranch == null && (rightBranch != null && rightBranch.getPriority() <= priority);
          }
          case Operator.OPTYPE_YF: {
            return (leftBranch != null && leftBranch.getPriority() <= priority) && rightBranch == null;
          }
          case Operator.OPTYPE_XF: {
            return (leftBranch != null && leftBranch.getPriority() < priority) && rightBranch == null;
          }
          case Operator.OPTYPE_XFX: {
            return (leftBranch != null && leftBranch.getPriority() < priority) && (rightBranch != null && rightBranch.getPriority() < priority);
          }
          case Operator.OPTYPE_XFY: {
            return (leftBranch != null && leftBranch.getPriority() < priority) && (rightBranch != null && rightBranch.getPriority() <= priority);
          }
          case Operator.OPTYPE_YFX: {
            return (leftBranch != null && leftBranch.getPriority() <= priority) && (rightBranch != null && rightBranch.getPriority() < priority);
          }
          default:
            throw new ProlCriticalError("Unknown operator type");
        }
      }
      else {
        return leftBranch == null && rightBranch == null;
      }
    }

    @Override
    public String toString() {
      return savedterm.toString();
    }

    /**
     * Make the tree item into a term
     *
     * @return the tree item converted into a term
     */
    private final Term convertTreeItemIntoTerm() {
      Term result = null;
      switch (savedterm.getTermType()) {
        case Term.TYPE_OPERATOR: {
          TermStruct operatorStruct = null;
          if (!validate()) {
            throw new ParserException("Wrong operator", lineNum, strPos);
          }

          final Term left = leftBranch != null ? leftBranch.convertTreeItemIntoTerm() : null;
          final Term right = rightBranch != null ? rightBranch.convertTreeItemIntoTerm() : null;
          if (left == null && right == null) {
            throw new ProlCriticalError("Operator without operands");
          }

          // this code replaces '-'(number) to '-number'
          if ("-".equals(savedterm.getText()) && left == null) {
            if (right.getTermType() == Term.TYPE_ATOM && right instanceof NumericTerm) {
              result = (Term) ((NumericTerm) right).neg();
              break;
            }
          }

          if (left != null) {
            if (right == null) {
              operatorStruct = new TermStruct((Operator) savedterm, new Term[]{left});
            }
            else {
              operatorStruct = new TermStruct((Operator) savedterm, new Term[]{left, right});
            }
          }
          else {
            operatorStruct = new TermStruct((Operator) savedterm, new Term[]{right});
          }

          operatorStruct.setPredicateProcessor(builder.context.findProcessor(operatorStruct));
          result = operatorStruct;
        }
        break;
        case Term.TYPE_STRUCT: {
          final TermStruct struct = (TermStruct) savedterm;
          struct.setPredicateProcessor(builder.context.findProcessor(struct));
          result = savedterm;
        }
        break;
        default: {
          result = savedterm;
        }
        break;
      }
      return result;
    }
  }

  /**
   * Check that an operator has been presented into an operator array
   *
   * @param operator the checked operator, it can be null so the result will be
   * true
   * @param endOperators the array to be checked that it contains such operator,
   * it can be null so the result will be false
   * @return true if the array contains the operator else false
   */
  private static final boolean isEndOperator(final Term operator, final OperatorContainer[] endOperators) {
    if (operator == null) {
      return true;
    }

    if (endOperators == null) {
      return false;
    }

    if (operator.getTermType() == Term.TYPE_OPERATORS) {
      final String operatorName = operator.getText();
      for (int li = 0; li < endOperators.length; li++) {
        if (endOperators[li].getText().equals(operatorName)) {
          return true;
        }
      }
    }
    return false;
  }

  /**
   * The map contains variable map which is being used to map variables inside a
   * tree
   */
  private final Map<String, Var> variableSet;

  /**
   * The tokenizer which will be used to get tokens to build the tree
   */
  private ProlTokenizer tokenizer;

  /**
   * The engine context which owns the tree builder
   */
  private final ProlContext context;

  /**
   * The knowledge base of the context
   */
  private final KnowledgeBase knowledgeBase;

  /**
   * The constructor
   *
   * @param context the context owns the tree builder, must not be null
   */
  public ProlTreeBuilder(final ProlContext context) {
    variableSet = new HashMap<String, Var>();
    this.context = context;
    knowledgeBase = context.getKnowledgeBase();

    OPERATORS_PHRASE = new OperatorContainer[]{context.getSystemOperatorForName(".")};
    OPERATORS_INSIDE_LIST = new OperatorContainer[]{context.getSystemOperatorForName(","), context.getSystemOperatorForName("]"), context.getSystemOperatorForName("|")};
    OPERATORS_END_LIST = new OperatorContainer[]{context.getSystemOperatorForName("]")};
    OPERATORS_INSIDE_STRUCT = new OperatorContainer[]{context.getSystemOperatorForName(","), context.getSystemOperatorForName(")")};
    OPERATORS_SUBBLOCK = new OperatorContainer[]{context.getSystemOperatorForName(")")};
  }

  /**
   * Parse a string and make a term tree from it.
   *
   * @param str the string to be parsed, must not be null
   * @return the term tree
   * @throws IOException it will be thrown it there will be any transport error
   * @throws InterruptedException it will be thrown if the thread has been
   * interrupted
   */
  public synchronized final Term readPhraseAndMakeTree(final String str) throws IOException, InterruptedException {
    return this.readPhraseAndMakeTree(new ProlReader(str));
  }

  /**
   * Read and make tree with predefined both a prol reader and a prol tokenizer
   *
   * @param tokenizer the tokenizer to be used for parsing, must not be null
   * @param reader the reader to be used for reading, must not be null
   * @return the term tree or null if the stream end has been reached
   * @throws IOException it will be thrown it there will be any transport error
   */
  public synchronized final Term readPhraseAndMakeTree(final ProlTokenizer tokenizer, final ProlReader reader) throws IOException {
    variableSet.clear();
    try {
      this.tokenizer = tokenizer;
      final Term result = readBlock(reader, OPERATORS_PHRASE);
      if (result == null) {
        return null; // end_of_file
      }
      final ProlTokenizerResult endAtom = tokenizer.nextToken(reader, knowledgeBase);
      if (endAtom == null || !endAtom.getText().equals(".")) {
        throw new ParserException("End operator is not found", reader.getLineNumber(), reader.getStrPos());
      }
      return result;
    }
    finally {
      variableSet.clear();
    }
  }

  /**
   * Read and make tree with a predefined prol reader
   *
   * @param reader the prol reader which will be used to read next token, must
   * not be null
   * @return the term tree
   * @throws IOException it will be thrown it there will be any transport error
   * @throws InterruptedException it will be thrown if the thread has been
   * interrupted
   */
  public synchronized final Term readPhraseAndMakeTree(final ProlReader reader) throws IOException, InterruptedException {
    return this.readPhraseAndMakeTree(new ProlTokenizer(), reader);
  }

  /**
   * Read a struct with a predefined functor from a reader
   *
   * @param functor the functor for the read structure, must not be null
   * @param reader the reader to read the structure, must not be null
   * @return read structure or null if the stream end has been reached
   * @throws IOException it will be thrown if there will be any transport error
   * @throws InterruptedException it will be thrown if the thread has been
   * interrupted
   */
  private final TermStruct readStruct(final Term functor, final ProlReader reader) throws IOException {
    final ArrayList<Term> listOfAtoms = new ArrayList<Term>();

    while (true) {
      final Term block = readBlock(reader, OPERATORS_INSIDE_STRUCT);

      final ProlTokenizerResult nextAtom = tokenizer.nextToken(reader, knowledgeBase);
      final String nextText = nextAtom.getText();

      if (",".equals(nextText)) {
        // next item
        if (block == null) {
          throw new ParserException("Empty structure element", tokenizer.getLastTokenLineNum(), tokenizer.getLastTokenStrPos());
        }
        else {
          listOfAtoms.add(block);
        }
        continue;
      }
      else if (")".equals(nextText)) {
        // end of the structure
        if (block != null) {
          listOfAtoms.add(block);
        }
        break;
      }
    }

    final TermStruct result = new TermStruct(functor, listOfAtoms.toArray(new Term[listOfAtoms.size()]));
    result.setPredicateProcessor(context.findProcessor(result));
    return result;
  }

  /**
   * Read a list with a predefined reader
   *
   * @param reader the reader which will be used to read the input stream, must
   * not be null
   * @return the read list or null if the stream end reached
   * @throws IOException it will be thrown if there is any transport error
   */
  private final Term readList(final ProlReader reader) throws IOException {
    TermList leftPart = TermList.NULLLIST;
    TermList leftPartFirst = leftPart;
    Term rightPart = null;

    boolean hasSeparator = false;

    boolean doRead = true;

    while (doRead) {
      final Term block = readBlock(reader, OPERATORS_INSIDE_LIST);

      ProlTokenizerResult nextAtom = tokenizer.nextToken(reader, knowledgeBase);

      final String text = nextAtom.getText();
      if ("]".equals(text)) {
        // end
        doRead = false;
        if (block == null) {
          continue;
        }
      }
      else if ("|".equals(text)) {
        // we have found the list tail, so we need read it as one block until the ']' atom
        if (block == null) {
          throw new ParserException("There is not any list element", tokenizer.getLastTokenLineNum(), tokenizer.getLastTokenStrPos());
        }
        if (leftPartFirst.isNullList()) {
          leftPartFirst = TermList.appendItem(leftPart, block);
          leftPart = leftPartFirst;
        }
        else {
          leftPart = TermList.appendItem(leftPart, block);
        }

        hasSeparator = true;

        rightPart = readBlock(reader, OPERATORS_END_LIST);
        nextAtom = tokenizer.nextToken(reader, knowledgeBase);
        if (!nextAtom.getText().equals("]")) {
          throw new ParserException("Wrong end of the list tail", tokenizer.getLastTokenLineNum(), tokenizer.getLastTokenStrPos());
        }

        break;
      }
      else if (",".equals(text)) {
        // all good and we read next block
        if (block == null) {
          throw new ParserException("List element not found", tokenizer.getLastTokenLineNum(), tokenizer.getLastTokenStrPos());
        }
      }
      else {
        throw new ProlCriticalError("Nonprocessd state at list definition");
      }

      if (leftPartFirst.isNullList()) {
        leftPartFirst = TermList.appendItem(leftPart, block);
        leftPart = leftPartFirst;
      }
      else {
        leftPart = TermList.appendItem(leftPart, block);
      }
    }

    if (hasSeparator) {
      // '|' separator was found at the list
      if (leftPart == null) {
        leftPart = TermList.NULLLIST;
      }
      if (rightPart == null) {
        throw new ParserException("There is not any term as the tail at the list", tokenizer.getLastTokenLineNum(), tokenizer.getLastTokenStrPos());
      }
      leftPartFirst.replaceLastElement(rightPart);
    }
    return leftPartFirst;
  }

  /**
   * Read a block from a predefined reader
   *
   * @param reader the predefined reader, must not be null
   * @param endOperators the array contains end operators which will be bounding
   * the block, it can be null but it will be no good indeed
   * @return a read block as Term or null if the end of the stream has been
   * reached
   * @throws IOException it will be thrown if there is any transport error
   */
  private final Term readBlock(final ProlReader reader, final OperatorContainer[] endOperators) throws IOException {
    // the variable will contain last processed tree item contains either atom or operator
    TreeItem currentTreeItem = null;

    while (true) {
      // read next atom from tokenizer
      ProlTokenizerResult readAtomContainer = tokenizer.nextToken(reader, knowledgeBase);
      boolean atBrakes = false;

      if (readAtomContainer == null) {
        if (currentTreeItem == null) {
          // end_of_file
          return null;
        }
        else {
          // non closed something
          throw new ParserException("Not-ended phrase", tokenizer.getLastTokenLineNum(), tokenizer.getLastTokenStrPos());
        }
      }

      Term readAtom = readAtomContainer.getTerm();

      // check the atom to be the end atom
      if (isEndOperator(readAtom, endOperators)) {
        // it's an end atom so we push it back and end the cycle
        tokenizer.pushTermBack(readAtomContainer);
        break;
      }

      // the variable contains calculated atem priority (it can be not the same as the nature priority)
      int readAtomPriority = 0; // we make it as zero (the highest priority) default

      // check read atom type
      if (readAtom.getTermType() == Term.TYPE_OPERATORS) {
        // it is operator list
        // try to get the single operator from the list if the linst contains only one
        final Operator readOperator = ((OperatorContainer) readAtom).getOperatorIfSingle();

        // check that the operator is single
        if (readOperator == null) {

          //there are a few operators in the list so we need to select one
          final OperatorContainer readOperators = (OperatorContainer) readAtom;

          boolean leftPresented = false;

          if (currentTreeItem != null) {
            if (currentTreeItem.getItemType() != Term.TYPE_OPERATOR) {
              leftPresented = true;
            }
            else {
              if (currentTreeItem.getRightBranch() != null) {
                leftPresented = true;
              }
            }
          }

          final boolean rightPresented = !isEndOperator(tokenizer.peekToken(reader, knowledgeBase).getTerm(), endOperators);

          readAtomContainer = null;
          readAtom = readOperators.getCompatibleOperator(leftPresented, rightPresented);

          if (readAtom == null) {
            // we didn't get any operator for our criteria, so throw an exception

            throw new ParserException("Incompatible operator type", tokenizer.getLastTokenLineNum(), tokenizer.getLastTokenStrPos());
          }
          // we have found needed operator so get its priority
          readAtomPriority = readAtom.getPriority();
        }
        else {
          readAtomContainer = null;
          readAtom = readOperator;
          final String operatorText = readOperator.getText();
          if (operatorText.length() == 1) {
            if ("[".equals(operatorText)) {
              // it's a list
              readAtom = readList(reader);
              readAtomPriority = 0;
            }
            else if ("(".equals(operatorText)) {
              // read subblock
              atBrakes = true;
              readAtomContainer = null;
              readAtom = readBlock(reader, OPERATORS_SUBBLOCK);
              readAtomPriority = 0;
              final Term closingAtom = tokenizer.nextToken(reader, knowledgeBase).getTerm();
              if (closingAtom == null || !closingAtom.getText().equals(")")) {
                throw new ParserException("Non-closed brakes", reader.getLineNumber(), reader.getStrPos());
              }
            }
            else {
              readAtomPriority = readOperator.getPriority();
            }
          }
          else {
            readAtomPriority = readOperator.getPriority();
          }
        }
      }
      else {
        final ProlTokenizerResult nextToken = tokenizer.nextToken(reader, knowledgeBase);
        if (nextToken != null && nextToken.getText().equals("(")) {
          // it is a structure
          if (readAtom.getTermType() == Term.TYPE_ATOM) {
            readAtom = readStruct(readAtom, reader);
          }
          else {
            tokenizer.pushTermBack(nextToken);
            throw new ParserException("You must have an atom as the structure functor", tokenizer.getLastTokenLineNum(), tokenizer.getLastTokenStrPos());
          }
        }
        else {
          // push back the next atom
          tokenizer.pushTermBack(nextToken);

          // check read atom to be zero-struct
          if (readAtomContainer.getState() == ProlTokenizerResult.STATE_ATOM && context.hasPredicateAtLibraryForSignature(readAtom.getText() + "/0")) {
            readAtomContainer = null;
            readAtom = new TermStruct(readAtom);
          }
        }
      }

      // check for variable
      if (readAtom.getTermType() == Term.TYPE_VAR) {
        // it's a variable
        final Var var = (Var) readAtom;
        if (!var.isAnonymous()) {
          // it's not an anonymous variable so we need to process it and cache if it is not at the var table yet
          final Var cachedVar = variableSet.get(var.getText());
          if (cachedVar == null) {
            // first meet variable
            // cache it
            variableSet.put(var.getText(), var);
          }
          else {
            // set cached variable instead of current value
            readAtom = cachedVar;
          }
        }
      }

      final TreeItem readAtomTreeItem = new TreeItem(this, readAtom, atBrakes, tokenizer.getLastTokenLineNum(), tokenizer.getLastTokenStrPos());

      if (currentTreeItem == null) {
        // it's first
        currentTreeItem = readAtomTreeItem;
      }
      else {
        // not first
        if (currentTreeItem.getItemType() == Term.TYPE_OPERATOR) {
          // it's an operator

          if (currentTreeItem.getPriority() <= readAtomPriority) {
            // new has low priority
            // make its as an ascendent
            final TreeItem foundItem = currentTreeItem.findFirstNodeWithSuchOrLowerPriority(readAtomPriority);
            if (foundItem.getPriority() < readAtomPriority) {
              // make as parent
              currentTreeItem = foundItem.makeAsOwnerWithLeftBranch(readAtomTreeItem);
            }
            else if (foundItem.getPriority() > readAtomPriority) {
              // make new as right subbranch
              currentTreeItem = foundItem.makeAsRightBranch(readAtomTreeItem);
            }
            else {
              // equals priority
              switch (foundItem.getOperatorType()) {
                case Operator.OPTYPE_XF:
                case Operator.OPTYPE_YF:
                case Operator.OPTYPE_FX:
                case Operator.OPTYPE_XFX:
                case Operator.OPTYPE_YFX: {
                  currentTreeItem = foundItem.makeAsOwnerWithLeftBranch(readAtomTreeItem);
                }
                break;
                case Operator.OPTYPE_FY:
                case Operator.OPTYPE_XFY: {
                  currentTreeItem = foundItem.makeAsRightBranch(readAtomTreeItem);
                }
                break;
                default:
                  throw new ProlCriticalError("Unknown operator type");
              }
            }

          }
          else if (currentTreeItem.getPriority() > readAtomPriority) {
            // new has great priority
            if (readAtomTreeItem.getItemType() != Term.TYPE_OPERATOR) {
              // it's a ground atom
              // so check that the right branch is empty
              if (currentTreeItem.getRightBranch() != null) {
                throw new ParserException("There is not any operator before the atom", tokenizer.getLastTokenLineNum(), tokenizer.getLastTokenStrPos());
              }
            }
            // make it as right
            currentTreeItem = currentTreeItem.makeAsRightBranch(readAtomTreeItem);
          }
        }
        else {
          // check that it is an operator
          if (currentTreeItem != null && currentTreeItem.getItemType() != Term.TYPE_OPERATOR && readAtomTreeItem.getItemType() != Term.TYPE_OPERATOR) {
            throw new ParserException("There must be an operator between atoms or structures", tokenizer.getLastTokenLineNum(), tokenizer.getLastTokenStrPos());
          }

          // make it as left branch
          currentTreeItem = currentTreeItem.makeAsOwnerWithLeftBranch(readAtomTreeItem);
        }
      }
    }
    if (currentTreeItem == null) {
      return null;
    }
    else {

      return currentTreeItem.findRoot().convertTreeItemIntoTerm();
    }
  }
}

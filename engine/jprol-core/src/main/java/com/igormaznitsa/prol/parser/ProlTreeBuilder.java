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

import com.igormaznitsa.prol.data.*;
import com.igormaznitsa.prol.exceptions.ParserException;
import com.igormaznitsa.prol.exceptions.ProlCriticalError;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.parser.ProlTokenizer.ProlTokenizerResult;
import com.igormaznitsa.prol.utils.Utils;
import com.igormaznitsa.prologparser.tokenizer.OpAssoc;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import static com.igormaznitsa.prol.data.TermType.*;
import static com.igormaznitsa.prol.data.Terms.newStruct;
import static com.igormaznitsa.prol.utils.Utils.createOrAppendToList;

public final class ProlTreeBuilder {

  private final TermOperatorContainer[] OPERATORS_PHRASE;
  private final TermOperatorContainer[] OPERATORS_INSIDE_LIST;
  private final TermOperatorContainer[] OPERATORS_END_LIST;
  private final TermOperatorContainer[] OPERATORS_INSIDE_STRUCT;
  private final TermOperatorContainer[] OPERATORS_SUBBLOCK;
  private final Map<String, TermVar> variableSet;
  private final ProlContext context;
  private ProlTokenizer tokenizer;

  public ProlTreeBuilder(final ProlContext context) {
    variableSet = new HashMap<>();
    this.context = context;

    OPERATORS_PHRASE = new TermOperatorContainer[] {context.getSystemOperatorForName(".")};
    OPERATORS_INSIDE_LIST = new TermOperatorContainer[] {context.getSystemOperatorForName(","), context.getSystemOperatorForName("]"), context.getSystemOperatorForName("|")};
    OPERATORS_END_LIST = new TermOperatorContainer[] {context.getSystemOperatorForName("]")};
    OPERATORS_INSIDE_STRUCT = new TermOperatorContainer[] {context.getSystemOperatorForName(","), context.getSystemOperatorForName(")")};
    OPERATORS_SUBBLOCK = new TermOperatorContainer[] {context.getSystemOperatorForName(")")};
  }

  private static boolean isEndOperator(final Term operator, final TermOperatorContainer[] endOperators) {
    if (operator == null) {
      return true;
    }

    if (endOperators == null) {
      return false;
    }

    if (operator.getTermType() == OPERATORS) {
      final String operatorName = operator.getText();
      for (TermOperatorContainer endOperator : endOperators) {
        if (endOperator.getText().equals(operatorName)) {
          return true;
        }
      }
    }
    return false;
  }

  public synchronized final Term readPhraseAndMakeTree(final String str) throws IOException {
    return this.readPhraseAndMakeTree(new ProlReader(str));
  }

  public synchronized final Term readPhraseAndMakeTree(final ProlTokenizer tokenizer, final ProlReader reader) throws IOException {
    variableSet.clear();
    try {
      this.tokenizer = tokenizer;
      final Term result = readBlock(reader, OPERATORS_PHRASE);
      if (result == null) {
        return null; // end_of_file
      }
      final ProlTokenizerResult endAtom = tokenizer.nextToken(reader, this.context);
      if (endAtom == null || !endAtom.getText().equals(".")) {
        throw new ParserException("End operator is not found", reader.getLineNumber(), reader.getStrPos());
      }
      return result;
    } finally {
      variableSet.clear();
    }
  }

  public synchronized final Term readPhraseAndMakeTree(final ProlReader reader) throws IOException {
    return this.readPhraseAndMakeTree(new ProlTokenizer(), reader);
  }

  private TermStruct readStruct(final Term functor, final ProlReader reader) throws IOException {
    final ArrayList<Term> listOfAtoms = new ArrayList<>();

    while (!Thread.currentThread().isInterrupted()) {
      final Term block = readBlock(reader, OPERATORS_INSIDE_STRUCT);

      final ProlTokenizerResult nextAtom = tokenizer.nextToken(reader, this.context);
      final String nextText = nextAtom.getText();

      if (",".equals(nextText)) {
        // next item
        if (block == null) {
          throw new ParserException("Empty structure element", tokenizer.getLastTokenLineNum(), tokenizer.getLastTokenStrPos());
        } else {
          listOfAtoms.add(block);
        }
      } else if (")".equals(nextText)) {
        // end of the structure
        if (block != null) {
          listOfAtoms.add(block);
        }
        break;
      }
    }

    final TermStruct result = newStruct(functor, listOfAtoms.toArray(new Term[0]));
    result.setPredicateProcessor(context.findProcessor(result));
    return result;
  }

  private Term readList(final ProlReader reader) throws IOException {
    TermList leftPart = Terms.NULL_LIST;
    TermList leftPartFirst = leftPart;
    Term rightPart = null;

    boolean hasSeparator = false;

    boolean doRead = true;

    OUTER:
    while (doRead) {
      final Term block = readBlock(reader, OPERATORS_INSIDE_LIST);
      ProlTokenizerResult nextAtom = tokenizer.nextToken(reader, this.context);
      final String text = nextAtom.getText();
      if (null == text) {
        throw new ProlCriticalError("Nonprocessd state at list definition");
      } else {
        switch (text) {
          case "]":
            // end
            doRead = false;
            if (block == null) {
              continue;
            }
            break;
          case "|":
            // we have found the list tail, so we need read it as one block until the ']' atom
            if (block == null) {
              throw new ParserException("There is not any list element", tokenizer.getLastTokenLineNum(), tokenizer.getLastTokenStrPos());
            }
            if (leftPartFirst.isNullList()) {
              leftPartFirst = createOrAppendToList(leftPart, block);
              leftPart = leftPartFirst;
            } else {
              leftPart = createOrAppendToList(leftPart, block);
            }
            hasSeparator = true;
            rightPart = readBlock(reader, OPERATORS_END_LIST);
            nextAtom = tokenizer.nextToken(reader, this.context);
            if (!nextAtom.getText().equals("]")) {
              throw new ParserException("Wrong end of the list tail", tokenizer.getLastTokenLineNum(), tokenizer.getLastTokenStrPos());
            }
            break OUTER;
          case ",":
            // all good and we read next block
            if (block == null) {
              throw new ParserException("List element not found", tokenizer.getLastTokenLineNum(), tokenizer.getLastTokenStrPos());
            }
            break;
          default:
            throw new ProlCriticalError("Nonprocessd state at list definition");
        }
      }
      if (leftPartFirst.isNullList()) {
        leftPartFirst = Utils.createOrAppendToList(leftPart, block);
        leftPart = leftPartFirst;
      } else {
        leftPart = Utils.createOrAppendToList(leftPart, block);
      }
    }

    if (hasSeparator) {
      // '|' separator was found at the list
      if (rightPart == null) {
        throw new ParserException("There is not any term as the tail at the list", tokenizer.getLastTokenLineNum(), tokenizer.getLastTokenStrPos());
      }
      leftPartFirst.replaceLastElement(rightPart);
    }
    return leftPartFirst;
  }

  private Term readBlock(final ProlReader reader, final TermOperatorContainer[] endOperators) throws IOException {
    // the variable will contain last processed tree item contains either atom or operator
    TreeItem currentTreeItem = null;

    while (!Thread.currentThread().isInterrupted()) {
      // read next atom from tokenizer
      ProlTokenizerResult readAtomContainer = tokenizer.nextToken(reader, this.context);
      boolean atBrakes = false;

      if (readAtomContainer == null) {
        if (currentTreeItem == null) {
          // end_of_file
          return null;
        } else {
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
      if (readAtom.getTermType() == OPERATORS) {
        // it is operator list
        // try to get the single operator from the list if the linst contains only one
        final TermOperator readOperator = ((TermOperatorContainer) readAtom).getOperatorIfSingle();

        // check that the operator is single
        if (readOperator == null) {

          //there are a few operators in the list so we need to select one
          final TermOperatorContainer readOperators = (TermOperatorContainer) readAtom;

          boolean leftPresented = false;

          if (currentTreeItem != null) {
            if (currentTreeItem.getItemType() != OPERATOR) {
              leftPresented = true;
            } else {
              if (currentTreeItem.getRightBranch() != null) {
                leftPresented = true;
              }
            }
          }

          final boolean rightPresented = !isEndOperator(tokenizer.peekToken(reader, this.context).getTerm(), endOperators);

          readAtom = readOperators.getCompatibleOperator(leftPresented, rightPresented);

          if (readAtom == null) {
            // we didn't get any operator for our criteria, so throw an exception

            throw new ParserException("Incompatible operator type", tokenizer.getLastTokenLineNum(), tokenizer.getLastTokenStrPos());
          }
          // we have found needed operator so get its priority
          readAtomPriority = readAtom.getPriority();
        } else {
          readAtom = readOperator;
          final String operatorText = readOperator.getText();
          if (operatorText.length() == 1) {
            switch (operatorText) {
              case "[":
                // it's a list
                readAtom = readList(reader);
                readAtomPriority = 0;
                break;
              case "(":
                // read subblock
                atBrakes = true;
                readAtom = readBlock(reader, OPERATORS_SUBBLOCK);
                readAtomPriority = 0;
                final Term closingAtom = tokenizer.nextToken(reader, this.context).getTerm();
                if (closingAtom == null || !closingAtom.getText().equals(")")) {
                  throw new ParserException("Non-closed brakes", reader.getLineNumber(), reader.getStrPos());
                }
                break;
              default:
                readAtomPriority = readOperator.getPriority();
                break;
            }
          } else {
            readAtomPriority = readOperator.getPriority();
          }
        }
      } else {
        final ProlTokenizerResult nextToken = tokenizer.nextToken(reader, this.context);
        if (nextToken != null && nextToken.getText().equals("(")) {
          // it is a structure
          if (readAtom.getTermType() == ATOM) {
            readAtom = readStruct(readAtom, reader);
          } else {
            tokenizer.pushTermBack(nextToken);
            throw new ParserException("You must have an atom as the structure functor", tokenizer.getLastTokenLineNum(), tokenizer.getLastTokenStrPos());
          }
        } else {
          // push back the next atom
          tokenizer.pushTermBack(nextToken);

          // check read atom to be zero-struct
          if (readAtomContainer.getState() == ProlTokenizerResult.STATE_ATOM && context.hasZeroArityPredicateForName(readAtom.getText())) {
            readAtom = newStruct(readAtom);
          }
        }
      }

      // check for variable
      if (readAtom.getTermType() == VAR) {
        // it's a variable
        final TermVar var = (TermVar) readAtom;
        if (!var.isAnonymous()) {
          // it's not an anonymous variable so we need to process it and cache if it is not at the var table yet
          final TermVar cachedVar = variableSet.get(var.getText());
          if (cachedVar == null) {
            // first meet variable
            // cache it
            variableSet.put(var.getText(), var);
          } else {
            // set cached variable instead of current value
            readAtom = cachedVar;
          }
        }
      }

      final TreeItem readAtomTreeItem = new TreeItem(this, readAtom, atBrakes, tokenizer.getLastTokenLineNum(), tokenizer.getLastTokenStrPos());

      if (currentTreeItem == null) {
        // it's first
        currentTreeItem = readAtomTreeItem;
      } else {
        // not first
        if (currentTreeItem.getItemType() == OPERATOR) {
          // it's an operator

          if (currentTreeItem.getPriority() <= readAtomPriority) {
            // new has low priority
            // make its as an ascendent
            final TreeItem foundItem = currentTreeItem.findFirstNodeWithSuchOrLowerPriority(readAtomPriority);
            if (foundItem.getPriority() < readAtomPriority) {
              // make as parent
              currentTreeItem = foundItem.makeAsOwnerWithLeftBranch(readAtomTreeItem);
            } else if (foundItem.getPriority() > readAtomPriority) {
              // make new as right subbranch
              currentTreeItem = foundItem.makeAsRightBranch(readAtomTreeItem);
            } else {
              // equals priority
              switch (foundItem.getOperatorType()) {
                case XF:
                case YF:
                case FX:
                case XFX:
                case YFX: {
                  currentTreeItem = foundItem.makeAsOwnerWithLeftBranch(readAtomTreeItem);
                }
                break;
                case FY:
                case XFY: {
                  currentTreeItem = foundItem.makeAsRightBranch(readAtomTreeItem);
                }
                break;
                default:
                  throw new ProlCriticalError("Unknown operator type");
              }
            }

          } else if (currentTreeItem.getPriority() > readAtomPriority) {
            // new has great priority
            if (readAtomTreeItem.getItemType() != OPERATOR) {
              // it's a ground atom
              // so check that the right branch is empty
              if (currentTreeItem.getRightBranch() != null) {
                throw new ParserException("There is not any operator before the atom", tokenizer.getLastTokenLineNum(), tokenizer.getLastTokenStrPos());
              }
            }
            // make it as right
            currentTreeItem = currentTreeItem.makeAsRightBranch(readAtomTreeItem);
          }
        } else {
          // check that it is an operator
          if (currentTreeItem.getItemType() != OPERATOR && readAtomTreeItem.getItemType() != OPERATOR) {
            throw new ParserException("There must be an operator between atoms or structures", tokenizer.getLastTokenLineNum(), tokenizer.getLastTokenStrPos());
          }

          // make it as left branch
          currentTreeItem = currentTreeItem.makeAsOwnerWithLeftBranch(readAtomTreeItem);
        }
      }
    }
    if (currentTreeItem == null) {
      return null;
    } else {

      return currentTreeItem.findRoot().convertTreeItemIntoTerm();
    }
  }

  private static final class TreeItem {
    private final ProlTreeBuilder builder;
    private final Term savedTerm;
    private final int strPos;
    private final int lineNum;
    private final boolean atBrakes;
    private TreeItem leftBranch;
    private TreeItem rightBranch;
    private TreeItem owner;

    private TreeItem(final ProlTreeBuilder builder, final Term term, final boolean atBrakes, final int lineNum, final int strPos) {
      savedTerm = term;
      this.builder = builder;
      this.strPos = strPos;
      this.lineNum = lineNum;
      this.atBrakes = atBrakes;
    }

    private int getPriority() {
      if (atBrakes) {
        return 0;
      }
      return savedTerm.getPriority();
    }

    private TreeItem makeAsRightBranch(final TreeItem item) {
      TreeItem currentSubbranch = rightBranch;
      setRightBranch(item);
      item.setLeftBranch(currentSubbranch);
      if (item.getItemType() == OPERATOR) {
        return item.getPriority() == 0 ? this : item;
      }
      return this;
    }

    private TreeItem makeAsOwnerWithLeftBranch(final TreeItem item) {
      this.replaceForOwner(item);
      item.setLeftBranch(this);
      return item;
    }

    private TreeItem getRightBranch() {
      return rightBranch;
    }

    private void setRightBranch(final TreeItem item) {
      rightBranch = item;
      if (item != null) {
        item.owner = this;
      }
    }

    private TreeItem getLeftBranch() {
      return leftBranch;
    }

    private void setLeftBranch(final TreeItem item) {
      leftBranch = item;
      if (item != null) {
        item.owner = this;
      }
    }

    private TermType getItemType() {
      return savedTerm.getTermType();
    }

    private TreeItem findRoot() {
      if (owner == null) {
        return this;
      }
      return owner.findRoot();
    }

    private TreeItem findFirstNodeWithSuchOrLowerPriority(final int priority) {
      final TreeItem result;
      if (getPriority() >= priority || owner == null) {
        result = this;
      } else {
        result = owner.findFirstNodeWithSuchOrLowerPriority(priority);
      }
      return result;
    }

    private void replaceForOwner(final TreeItem newItem) {
      if (owner == null) {
        newItem.owner = null;
        return;
      }

      if (this.equals(owner.getLeftBranch())) {
        owner.setLeftBranch(newItem);
      } else {
        owner.setRightBranch(newItem);
      }
    }

    private OpAssoc getOperatorType() {
      return ((TermOperator) savedTerm).getOperatorType();
    }

    private boolean validate() {
      if (savedTerm.getTermType() == OPERATOR) {
        final int priority = getPriority();

        switch (((TermOperator) savedTerm).getOperatorType()) {
          case FX: {
            return leftBranch == null && (rightBranch != null && rightBranch.getPriority() < priority);
          }
          case FY: {
            return leftBranch == null && (rightBranch != null && rightBranch.getPriority() <= priority);
          }
          case YF: {
            return (leftBranch != null && leftBranch.getPriority() <= priority) && rightBranch == null;
          }
          case XF: {
            return (leftBranch != null && leftBranch.getPriority() < priority) && rightBranch == null;
          }
          case XFX: {
            return (leftBranch != null && leftBranch.getPriority() < priority) && (rightBranch != null && rightBranch.getPriority() < priority);
          }
          case XFY: {
            return (leftBranch != null && leftBranch.getPriority() < priority) && (rightBranch != null && rightBranch.getPriority() <= priority);
          }
          case YFX: {
            return (leftBranch != null && leftBranch.getPriority() <= priority) && (rightBranch != null && rightBranch.getPriority() < priority);
          }
          default:
            throw new ProlCriticalError("Unknown operator type");
        }
      } else {
        return leftBranch == null && rightBranch == null;
      }
    }

    @Override
    public String toString() {
      return savedTerm.toString();
    }

    private Term convertTreeItemIntoTerm() {
      Term result;
      switch (savedTerm.getTermType()) {
        case OPERATOR: {
          final TermStruct operatorStruct;
          if (!validate()) {
            throw new ParserException("Wrong operator", lineNum, strPos);
          }

          final Term left = leftBranch != null ? leftBranch.convertTreeItemIntoTerm() : null;
          final Term right = rightBranch != null ? rightBranch.convertTreeItemIntoTerm() : null;
          if (left == null && right == null) {
            throw new ProlCriticalError("TermOperator without operands");
          }

          // this code replaces '-'(number) to '-number'
          if ("-".equals(savedTerm.getText()) && left == null) {
            if (right instanceof NumericTerm && right.getTermType() == ATOM) {
              result = ((NumericTerm) right).neg();
              break;
            }
          }

          if (left != null) {
            if (right == null) {
              operatorStruct = newStruct(savedTerm, new Term[] {left});
            } else {
              operatorStruct = newStruct(savedTerm, new Term[] {left, right});
            }
          } else {
            operatorStruct = newStruct(savedTerm, new Term[] {right});
          }

          operatorStruct.setPredicateProcessor(builder.context.findProcessor(operatorStruct));
          result = operatorStruct;
        }
        break;
        case STRUCT: {
          final TermStruct struct = (TermStruct) savedTerm;
          struct.setPredicateProcessor(builder.context.findProcessor(struct));
          result = savedTerm;
        }
        break;
        default: {
          result = savedTerm;
        }
        break;
      }
      return result;
    }
  }
}

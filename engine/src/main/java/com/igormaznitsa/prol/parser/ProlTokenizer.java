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
import com.igormaznitsa.prol.containers.OperatorContainer;
import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.TermFloat;
import com.igormaznitsa.prol.data.TermInteger;
import com.igormaznitsa.prol.data.Var;
import com.igormaznitsa.prol.exceptions.ProlCriticalError;
import com.igormaznitsa.prol.exceptions.ParserException;
import java.io.IOException;

/**
 * The class implements a tokenizer which can parse a prolog source
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public final class ProlTokenizer {

  /**
   * Inside class which used to present a token read from the source input
   * stream
   *
   * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
   */
  public final static class ProlTokenizerResult {

    /**
     * A type for the result. The type shows the "look for" mode
     */
    public static final int STATE_LOOKFOR = 0;
    /**
     * A type for the result. The type shows that a text atom has been found
     */
    public static final int STATE_ATOM = 1;
    /**
     * A type for the result. The type shows that a string (an atom bounded by
     * \') has been found
     */
    public static final int STATE_STRING = 2;
    /**
     * A type for the result. The type shows that an operator has been found
     * (the operator has been found at current context)
     */
    public static final int STATE_OPERATOR = 3;
    /**
     * A type for the result. The type shows that a variable has been found
     */
    public static final int STATE_VARIABLE = 4;
    /**
     * The variable contains read atom
     */
    private final Term term;
    /**
     * The variable contains the state which was associated by the state machine
     * with the read atom
     */
    private final int state;

    /**
     * The constructor
     *
     * @param term the read term, must not be null
     * @param state the state of the state machine
     */
    public ProlTokenizerResult(final Term term, final int state) {
      this.term = term;
      this.state = state;
    }

    /**
     * Get the result term type
     *
     * @return the result term type as integer
     * @see com.igormaznitsa.prol.data.Term
     */
    public final int getTermType() {
      return term.getTermType();
    }

    /**
     * Get the text of read term
     *
     * @return the text of the read term
     */
    public final String getText() {
      return term.getText();
    }

    /**
     * Get the state of the state machine associated with te read term
     *
     * @return the state as integer
     */
    public final int getState() {
      return state;
    }

    /**
     * Get the read term
     *
     * @return the read term
     */
    public final Term getTerm() {
      return term;
    }
  }
  /**
   * The variable contains the last pushed term. The term had been read but the
   * reader pushed it back to reread it lately
   */
  private ProlTokenizerResult lastPushedTerm;
  /**
   * The variable saves the previous value of the read token line number
   */
  private int prevReadTokenLineNum;
  /**
   * The variable saves the previous value of the read token string position
   */
  private int prevReadTokenStrPos;
  /**
   * The variable saves the last value of the read token line number
   */
  private int lastReadTokenLineNum;
  /**
   * The variable saves the last value of the read token string position
   */
  private int lastReadTokenStrPos;
  /**
   * Inside state for the state machine shows that the state machine is looking
   * for the next token
   */
  private static final int INSIDE_STATE_LOOKFOR = 0;
  /**
   * Inside state for the state machine shows that the state machine has an atom
   * in its buffer
   */
  private static final int INSIDE_STATE_ATOM = 1;
  /**
   * Inside state for the state machine shows that the state machine has a
   * string in its buffer
   */
  private static final int INSIDE_STATE_STRING = 2;
  /**
   * Inside state for the state machine shows that the state machine has an
   * operator in its buffer
   */
  private static final int INSIDE_STATE_OPERATOR = 3;
  /**
   * Inside state for the state machine shows that the state machine has a
   * variable in its buffer
   */
  private static final int INSIDE_STATE_VARIABLE = 4;
  /**
   * Inside state for the state machine shows that the state machine has an
   * integer value in its buffer
   */
  private static final int INSIDE_STATE_INTEGER = 5;
  /**
   * Inside state for the state machine shows that the state machine has an
   * float value in its buffer
   */
  private static final int INSIDE_STATE_FLOAT = 6;

  /**
   * The constructor
   */
  public ProlTokenizer() {
    super();
  }

  /**
   * Push a read object back into buffer to read it lately
   *
   * @param object the object to be pushed back into buffer, null will clear the
   * buffer
   */
  public void pushTermBack(final ProlTokenizerResult object) {
    if (lastPushedTerm != null) {
      throw new IllegalStateException("An object has been pushed already");
    }
    lastPushedTerm = object;
  }

  /**
   * Peek the next token from the incomming stream. The token will be read and
   * available but it will not be removed from the incomming stream.
   *
   * @param reader the reader to get the incomming token, must not be null
   * @param voc the knowledge base which will be used for the operation, must
   * not be null
   * @return a read token as a ProlTokenizerResult, or null if there is not any
   * token in the stream
   * @throws IOException it will be throws if there is any transport problem
   */
  public ProlTokenizerResult peekToken(final ProlReader reader, final KnowledgeBase voc) throws IOException {
    ProlTokenizerResult result = null;
    if (lastPushedTerm == null) {
      result = nextToken(reader, voc);
      pushTermBack(result);
    }
    else {
      result = lastPushedTerm;
    }
    return result;
  }

  /**
   * Get the last string position of the read token
   *
   * @return the last string position for the read token as integer
   */
  public int getLastTokenStrPos() {
    return lastPushedTerm == null ? lastReadTokenStrPos : prevReadTokenStrPos;
  }

  /**
   * Get the last line number for the read token
   *
   * @return the last line number for the read token as integer
   */
  public int getLastTokenLineNum() {
    return lastPushedTerm == null ? lastReadTokenLineNum : prevReadTokenLineNum;
  }

  /**
   * Inside function to fix current read position of string and line numbers
   *
   * @param reader the reader which position shoul be fixed in the inside
   * variables, must not be null
   */
  private void fixPosition(final ProlReader reader) {
    prevReadTokenLineNum = lastReadTokenLineNum;
    prevReadTokenStrPos = lastReadTokenStrPos;
    lastReadTokenLineNum = reader.getLineNumber();
    lastReadTokenStrPos = reader.getStrPos();
  }

  /**
   * Skip all comments (started with %) in the incomming stream
   *
   * @param reader the reader whose comments should be skipped, must nit be null
   * @throws IOException it will be thrown if there will be any transport
   * problem during the operation
   */
  private void skipComments(final ProlReader reader) throws IOException {
    while (true) {
      final int readchar = reader.read();
      if (readchar < 0 || readchar == '\n') {
        break;
      }
    }
  }

  /**
   * Read next token from a reader
   *
   * @param reader the reader which will be used to read next token, must not be
   * null
   * @param voc the knowledge base which will be used for the operation, must
   * not be null
   * @return next token as a ProlTokenizerResult object
   * @throws IOException it will be thrown if there is any transport error
   * during the operation
   */
  public ProlTokenizerResult nextToken(final ProlReader reader, final KnowledgeBase voc) throws IOException {

    if (lastPushedTerm != null) {
      try {
        return lastPushedTerm;
      }
      finally {
        lastPushedTerm = null;
      }
    }

    int state = INSIDE_STATE_LOOKFOR;
    boolean specialchar = false;

    final StringBuilder strbuffer = new StringBuilder();

    OperatorContainer lastFoundFullOperator = null;

    boolean letterOrDigitOnly = false;

    while (true) {
      final int readchar = reader.read();

      if (readchar < 0) {
        final String str = strbuffer.toString();
        switch (state) {
          case INSIDE_STATE_LOOKFOR:
            return null;
          case INSIDE_STATE_FLOAT: {
            if (str.charAt(str.length() - 1) == '.') {
              // non ended float then it integer + '.'
              reader.pushCharBack('.');
              // it is Integer
              return new ProlTokenizerResult(makeTermFromString(str.substring(0, str.length() - 1), INSIDE_STATE_INTEGER), INSIDE_STATE_ATOM);
            }
          }
          case INSIDE_STATE_INTEGER:
            return new ProlTokenizerResult(makeTermFromString(str, state), INSIDE_STATE_ATOM);
          case INSIDE_STATE_ATOM:
            return new ProlTokenizerResult(makeTermFromString(str, state), INSIDE_STATE_ATOM);
          case INSIDE_STATE_VARIABLE:
            if (str.equals("_")) {
              return new ProlTokenizerResult(new Var(), state);
            }
            else {
              return new ProlTokenizerResult(new Var(str), state);
            }

          case INSIDE_STATE_STRING:
            throw new ParserException("Unclosed string found", lastReadTokenLineNum, lastReadTokenStrPos);
          case INSIDE_STATE_OPERATOR: {
            if (lastFoundFullOperator == null) {
              return new ProlTokenizerResult(makeTermFromString(str, state), state);
            }
            else {
              reader.pushBufferDifference(lastFoundFullOperator.getText(), strbuffer);
              return new ProlTokenizerResult(lastFoundFullOperator, state);
            }

          }
          default:
            throw new ProlCriticalError("Unknown reader state");
        }
      }

      final char chr = (char) readchar;

      switch (state) {
        case INSIDE_STATE_LOOKFOR: {
          if (Character.isISOControl(chr) || Character.isWhitespace(chr)) {
            continue;
          }

          switch (chr) {
            case '%': {
              // comments
              skipComments(reader);
            }
            break;
            case '_': {
              fixPosition(reader);
              strbuffer.append(chr);
              state = INSIDE_STATE_VARIABLE;
            }
            break;
            case '\'': {
              fixPosition(reader);
              state = INSIDE_STATE_STRING;
            }
            break;

            default: {
              fixPosition(reader);

              strbuffer.append(chr);

              if (Character.isLetter(chr) && Character.isUpperCase(chr)) {
                state = INSIDE_STATE_VARIABLE;
              }
              else {
                letterOrDigitOnly = Character.isLetterOrDigit(chr);
                String operator = Character.toString(chr);
                if (voc.hasOperatorStartsWith(operator)) {
                  lastFoundFullOperator = voc.findOperatorForName(operator);
                  state = INSIDE_STATE_OPERATOR;
                }
                else {
                  if (Character.isDigit(chr)) {
                    state = INSIDE_STATE_INTEGER;
                  }
                  else {
                    state = INSIDE_STATE_ATOM;
                  }
                }
              }
            }
          }
        }
        break;
        case INSIDE_STATE_ATOM: {
          if (chr == '_') {
            strbuffer.append(chr);
          }
          else if (Character.isWhitespace(chr) || Character.isISOControl(chr)) {
            return new ProlTokenizerResult(makeTermFromString(strbuffer.toString(), state), state);
          }
          else if (chr == '\'' || (letterOrDigitOnly != Character.isLetterOrDigit(chr)) || voc.findOperatorForName(Character.toString(chr)) != null) {
            reader.pushCharBack(chr);
            return new ProlTokenizerResult(makeTermFromString(strbuffer.toString(), state), state);
          }
          else {
            strbuffer.append(chr);
          }
        }
        break;
        case INSIDE_STATE_INTEGER: {
          if (Character.isDigit(chr)) {
            strbuffer.append(chr);
          }
          else {
            if (chr == '.' || chr == 'e' || chr == 'E') {
              strbuffer.append(chr);
              state = INSIDE_STATE_FLOAT;
            }
            else {
              reader.pushCharBack(chr);
              return new ProlTokenizerResult(makeTermFromString(strbuffer.toString(), state), INSIDE_STATE_ATOM);
            }
          }
        }
        break;
        case INSIDE_STATE_FLOAT: {
          if (Character.isDigit(chr)) {
            strbuffer.append(chr);
          }
          else {
            if (chr == '-' || chr == '+') {
              if (strbuffer.charAt(strbuffer.length() - 1) == 'e') {
                strbuffer.append(chr);
              }
              else {
                reader.pushCharBack(chr);
                return new ProlTokenizerResult(makeTermFromString(strbuffer.toString(), INSIDE_STATE_FLOAT), INSIDE_STATE_ATOM);
              }
            }
            else if (chr == 'e' || chr == 'E') {
              if (strbuffer.indexOf("e") < 0) {
                strbuffer.append('e');
              }
              else {
                reader.pushCharBack(chr);
                return new ProlTokenizerResult(makeTermFromString(strbuffer.substring(0, strbuffer.length() - 1), INSIDE_STATE_FLOAT), INSIDE_STATE_ATOM);
              }
            }
            else {

              reader.pushCharBack(chr);

              if (strbuffer.charAt(strbuffer.length() - 1) == '.') {
                // it was an integer
                reader.pushCharBack('.');
                return new ProlTokenizerResult(makeTermFromString(strbuffer.substring(0, strbuffer.length() - 1), INSIDE_STATE_INTEGER), INSIDE_STATE_ATOM);
              }
              else {
                // it is float
                return new ProlTokenizerResult(makeTermFromString(strbuffer.toString(), state), INSIDE_STATE_ATOM);
              }
            }
          }
        }
        break;
        case INSIDE_STATE_OPERATOR: {
          if (chr != '_' && letterOrDigitOnly != Character.isLetterOrDigit(chr)) {
            reader.pushCharBack(chr);

            if (lastFoundFullOperator != null) {
              return new ProlTokenizerResult(lastFoundFullOperator, state);
            }
            else {
              return new ProlTokenizerResult(makeTermFromString(strbuffer.toString(), state), state);
            }
          }
          else {
            final OperatorContainer prevoperators = lastFoundFullOperator;
            strbuffer.append(chr);
            final String operator = strbuffer.toString();
            lastFoundFullOperator = voc.findOperatorForName(operator);
            if (prevoperators != null) {
              if (lastFoundFullOperator == null) {
                if (!voc.hasOperatorStartsWith(operator)) {
                  if (letterOrDigitOnly) {
                    state = INSIDE_STATE_ATOM;
                  }
                  else {
                    reader.pushBufferDifference(prevoperators.getText(), strbuffer);
                    return new ProlTokenizerResult(prevoperators, state);
                  }
                }
                else {
                  lastFoundFullOperator = prevoperators;
                }

              }
              else {
                if (!voc.hasOperatorStartsWith(operator)) {
                  reader.pushBufferDifference(prevoperators.getText(), strbuffer);
                  return new ProlTokenizerResult(prevoperators, state);
                }
              }
            }
            else {
              if (!voc.hasOperatorStartsWith(operator)) {
                if (voc.hasOperatorStartsWith(Character.toString(chr))) {
                  // next char can be the start char of an operator so we need get back it into the buffer
                  strbuffer.setLength(strbuffer.length() - 1);
                  reader.pushCharBack(chr);
                }
                state = INSIDE_STATE_ATOM;
              }
            }
          }
        }
        break;
        case INSIDE_STATE_STRING: {
          if (specialchar) {
            switch (chr) {
              case '\'':
                strbuffer.append('\'');
                break;

              case '\"':
                strbuffer.append('\"');
                break;

              case 'n':
                strbuffer.append('\n');
                break;

              case 'f':
                strbuffer.append('\f');
                break;

              case 'r':
                strbuffer.append('\r');
                break;

              case 't':
                strbuffer.append('\t');
                break;

              case '\\':
                strbuffer.append('\\');
                break;

              default:
                throw new ParserException("Unsupported special char", reader.getPrevLineNumber(), reader.getPrevStrPos());
            }
            specialchar = false;
          }
          else {
            switch (chr) {
              case '\'':
                return new ProlTokenizerResult(makeTermFromString(strbuffer.toString(), state), state);
              case '\\': {
                specialchar = true;
              }
              break;
              default: {
                strbuffer.append(chr);
              }

            }
          }
        }
        break;
        case INSIDE_STATE_VARIABLE: {
          if (Character.isISOControl(chr) || Character.isWhitespace(chr)) {
            final String name = strbuffer.toString();
            if (name.equals("_")) {
              return new ProlTokenizerResult(new Var(), state);
            }
            return new ProlTokenizerResult(new Var(name), state);
          }
          else if (chr != '_' && !Character.isLetterOrDigit(chr)) {
            reader.pushCharBack(chr);
            final String name = strbuffer.toString();
            if (name.equals("_")) {
              return new ProlTokenizerResult(new Var(), state);
            }
            return new ProlTokenizerResult(new Var(name), state);
          }
          else {
            strbuffer.append(chr);
          }
        }
        break;
      }
    }
  }

  /**
   * Inside auxulary function to make a term from a String
   *
   * @param string the source string object, must not be null
   * @param state the state of inside state machine which was used to read the
   * term
   * @return a Term object as the result, must not be null
   */
  private Term makeTermFromString(final String string, final int state) {
    Term result = null;

    switch (state) {
      case INSIDE_STATE_INTEGER: {
        try {
          result = new TermInteger(string);
        }
        catch (NumberFormatException ex) {
        }
      }
      break;
      case INSIDE_STATE_FLOAT: {
        try {
          result = new TermFloat(string);
        }
        catch (NumberFormatException ex) {
        }
      }
      break;
    }

    if (result == null) {
      result = new Term(string);
    }

    return result;
  }
}

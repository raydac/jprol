package com.igormaznitsa.jprol.easygui.tokenizer;

import javax.swing.text.Segment;
import org.fife.ui.rsyntaxtextarea.AbstractTokenMaker;
import org.fife.ui.rsyntaxtextarea.RSyntaxUtilities;
import org.fife.ui.rsyntaxtextarea.Token;
import org.fife.ui.rsyntaxtextarea.TokenMap;

public class JProlTokenMaker extends AbstractTokenMaker {

  public static final String MIME = "text/jprol";
  protected final String separators = "()[]{}|,.";

  private int currentTokenStart;
  private int currentTokenType;

  public JProlTokenMaker() {
    super();
  }

  /**
   * Checks the token to give it the exact ID it deserves before being passed up
   * to the super method.
   *
   * @param segment     <code>Segment</code> to get text from.
   * @param start       Start offset in <code>segment</code> of token.
   * @param end         End offset in <code>segment</code> of token.
   * @param tokenType   The token's type.
   * @param startOffset The offset in the document at which the token occurs.
   */
  @Override
  public void addToken(Segment segment, int start, int end, int tokenType, int startOffset) {

    switch (tokenType) {
      // Since reserved words, functions, and data types are all passed into here
      // as "identifiers," we have to see what the token really is...
      case Token.IDENTIFIER:
        int value = wordsToHighlight.get(segment, start, end);
        if (value != -1) {
          tokenType = value;
        }
        break;
      case Token.WHITESPACE:
      case Token.SEPARATOR:
      case Token.OPERATOR:
      case Token.LITERAL_NUMBER_DECIMAL_INT:
      case Token.LITERAL_STRING_DOUBLE_QUOTE:
      case Token.LITERAL_CHAR:
      case Token.COMMENT_EOL:
      case Token.PREPROCESSOR:
      case Token.VARIABLE:
        break;

      default:
        tokenType = Token.IDENTIFIER;
        break;

    }

    super.addToken(segment, start, end, tokenType, startOffset);

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String[] getLineCommentStartAndEnd(int languageIndex) {
    return new String[] {"%", null};
  }

  /**
   * Returns whether tokens of the specified type should have "mark occurrences"
   * enabled for the current programming language.
   *
   * @param type The token type.
   * @return Whether tokens of this type should have "mark occurrences" enabled.
   */
  @Override
  public boolean getMarkOccurrencesOfTokenType(int type) {
    return type == Token.IDENTIFIER || type == Token.VARIABLE;
  }

  /**
   * Returns the words to highlight for UNIX shell scripts.
   *
   * @return A <code>TokenMap</code> containing the words to highlight for UNIX
   * shell scripts.
   * @see org.fife.ui.rsyntaxtextarea.AbstractTokenMaker#getWordsToHighlight
   */
  @Override
  public TokenMap getWordsToHighlight() {

    TokenMap tokenMap = new TokenMap();

    int reservedWord = Token.RESERVED_WORD;
    tokenMap.put("not", reservedWord);
    tokenMap.put("is", reservedWord);
    tokenMap.put("fail", reservedWord);
    tokenMap.put("repeat", reservedWord);
    tokenMap.put("true", reservedWord);
    tokenMap.put("false", reservedWord);
    tokenMap.put("div", reservedWord);
    tokenMap.put("mod", reservedWord);
    tokenMap.put("quot", reservedWord);
    tokenMap.put("rem", reservedWord);
    tokenMap.put("!", reservedWord);

    int function = Token.FUNCTION;
    tokenMap.put("op", function);

    int operator = Token.OPERATOR;
    tokenMap.put("?-", operator);
    tokenMap.put("^", operator);
    tokenMap.put(";", operator);
    tokenMap.put("|", operator);
    tokenMap.put("::", operator);
    tokenMap.put("+", operator);
    tokenMap.put("\\", operator);
    tokenMap.put("-", operator);
    tokenMap.put("=", operator);
    tokenMap.put("\\=", operator);
    tokenMap.put("=..", operator);
    tokenMap.put("<<", operator);
    tokenMap.put(">>", operator);
    tokenMap.put("==", operator);
    tokenMap.put("\\==", operator);
    tokenMap.put("@<", operator);
    tokenMap.put("@=<", operator);
    tokenMap.put("@>", operator);
    tokenMap.put("@>=", operator);
    tokenMap.put("<", operator);
    tokenMap.put("=<", operator);
    tokenMap.put(">", operator);
    tokenMap.put(">=", operator);
    tokenMap.put("=:=", operator);
    tokenMap.put("=\\=", operator);
    tokenMap.put("is", operator);
    tokenMap.put("*", operator);
    tokenMap.put("/", operator);
    tokenMap.put("**", operator);
    tokenMap.put("//", operator);
    tokenMap.put(":-", operator);
    tokenMap.put("-->", operator);
    tokenMap.put("/\\", operator);
    tokenMap.put("\\/", operator);
    tokenMap.put("\\/", operator);

    return tokenMap;

  }

  /**
   * Returns a list of tokens representing the given text.
   *
   * @param text           The text to break into tokens.
   * @param startTokenType The token with which to start tokenizing.
   * @param startOffset    The offset at which the line of tokens begins.
   * @return A linked list of tokens representing <code>text</code>.
   */
  @Override
  public Token getTokenList(Segment text, int startTokenType, final int startOffset) {

    resetTokenList();

    char[] array = text.array;
    int offset = text.offset;
    int count = text.count;
    int end = offset + count;

    // See, when we find a token, its starting position is always of the form:
    // 'startOffset + (currentTokenStart-offset)'; but since startOffset and
    // offset are constant, tokens' starting positions become:
    // 'newStartOffset+currentTokenStart' for one less subtraction operation.
    int newStartOffset = startOffset - offset;

    currentTokenStart = offset;
    currentTokenType = startTokenType;
    boolean backslash = false;

    for (int i = offset; i < end; i++) {
      char c = array[i];
      switch (currentTokenType) {
        case Token.NULL: {
          currentTokenStart = i;
          switch (c) {
            case ' ':
            case '\t': {
              currentTokenType = Token.WHITESPACE;
            }
            break;
            case '\'': {
              if (backslash) {
                addToken(text, currentTokenStart, i, Token.IDENTIFIER,
                    newStartOffset + currentTokenStart);
                backslash = false;
              } else {
                currentTokenType = Token.LITERAL_STRING_DOUBLE_QUOTE;
              }
            }
            break;
            case '\\': {
              addToken(text, currentTokenStart, i, Token.IDENTIFIER,
                  newStartOffset + currentTokenStart);
              currentTokenType = Token.NULL;
              backslash = !backslash;
            }
            break;
            case '_': {
              if (backslash) {
                addToken(text, currentTokenStart, i, Token.IDENTIFIER,
                    newStartOffset + currentTokenStart);
                backslash = false;
              } else {
                currentTokenType = Token.VARIABLE;
              }
            }
            break;
            case '%': {
              backslash = false;
              currentTokenType = Token.COMMENT_EOL;
            }
            break;
            default: {
              if (RSyntaxUtilities.isLetter(c) && Character.isUpperCase(c)) {
                currentTokenType = Token.VARIABLE;
                break;
              }
              if (RSyntaxUtilities.isDigit(c)) {
                currentTokenType = Token.LITERAL_NUMBER_DECIMAL_INT;
                break;
              } else if (RSyntaxUtilities.isLetter(c) || c == '/') {
                currentTokenType = Token.IDENTIFIER;
                break;
              }
              int indexOf = separators.indexOf(c);
              if (indexOf > -1) {
                addToken(text, currentTokenStart, i, Token.SEPARATOR,
                    newStartOffset + currentTokenStart);
                currentTokenType = Token.NULL;
                break;
              } else {
                currentTokenType = Token.FUNCTION;
              }
            }
            break;
          }
        }
        break;
        case Token.WHITESPACE: {
          switch (c) {
            case ' ':
            case '\t':
              break;
            case '\'': {// Don't need to worry about backslashes as previous char is space.
              addToken(text, currentTokenStart, i - 1, Token.WHITESPACE,
                  newStartOffset + currentTokenStart);
              currentTokenStart = i;
              currentTokenType = Token.LITERAL_STRING_DOUBLE_QUOTE;
              backslash = false;
            }
            break;
            case '_': { // Don't need to worry about backslashes as previous char is space.
              addToken(text, currentTokenStart, i - 1, Token.WHITESPACE,
                  newStartOffset + currentTokenStart);
              currentTokenStart = i;
              currentTokenType = Token.VARIABLE;
              backslash = false;
            }
            break;
            case '%': {
              addToken(text, currentTokenStart, i - 1, Token.WHITESPACE,
                  newStartOffset + currentTokenStart);
              currentTokenStart = i;
              currentTokenType = Token.COMMENT_EOL;
            }
            break;
            default: {    // Add the whitespace token and start anew.
              addToken(text, currentTokenStart, i - 1, Token.WHITESPACE,
                  newStartOffset + currentTokenStart);
              currentTokenStart = i;

              if (RSyntaxUtilities.isLetter(c) && Character.isUpperCase(c)) {
                currentTokenType = Token.VARIABLE;
                break;
              }

              if (RSyntaxUtilities.isDigit(c)) {
                currentTokenType = Token.LITERAL_NUMBER_DECIMAL_INT;
                break;
              } else if (RSyntaxUtilities.isLetter(c) || c == '/') {
                currentTokenType = Token.IDENTIFIER;
                break;
              }
              int indexOf = separators.indexOf(c);
              if (indexOf > -1) {
                addToken(text, i, i, Token.SEPARATOR, newStartOffset + i);
                currentTokenType = Token.NULL;
                break;
              } else {
                currentTokenType = Token.FUNCTION;
              }
            }
          }
        }
        break;
        case Token.FUNCTION:
        case Token.IDENTIFIER: {
          switch (c) {
            case ' ':
            case '\t': {
              addToken(text, currentTokenStart, i - 1, Token.IDENTIFIER,
                  newStartOffset + currentTokenStart);
              currentTokenStart = i;
              currentTokenType = Token.WHITESPACE;
            }
            break;
            case '/': {
              if (currentTokenType != Token.FUNCTION) {
                addToken(text, currentTokenStart, i, Token.IDENTIFIER,
                    newStartOffset + currentTokenStart);
                currentTokenStart = i + 1;
                currentTokenType = Token.NULL;
              }
            }
            break;
            case '\'': {
              addToken(text, currentTokenStart, i - 1, Token.IDENTIFIER,
                  newStartOffset + currentTokenStart);
              currentTokenStart = i;
              currentTokenType = Token.LITERAL_STRING_DOUBLE_QUOTE;
              backslash = false;
            }
            break;
            case '\\': {
              if (currentTokenType != Token.FUNCTION) {
                addToken(text, currentTokenStart, i - 1, Token.IDENTIFIER,
                    newStartOffset + currentTokenStart);
                addToken(text, i, i, Token.IDENTIFIER, newStartOffset + i);
                currentTokenType = Token.NULL;
                backslash = true;
              }
            }
            break;
            default: {
              if (currentTokenType == Token.FUNCTION) {
                if (RSyntaxUtilities.isLetterOrDigit(c) || c == '_') {
                  addToken(text, currentTokenStart, i - 1, Token.IDENTIFIER,
                      newStartOffset + currentTokenStart);
                  i--;
                  currentTokenType = Token.NULL;
                  break;
                }
                int indexOf = separators.indexOf(c);
                if (indexOf > -1) {
                  addToken(text, currentTokenStart, i - 1, Token.IDENTIFIER,
                      newStartOffset + currentTokenStart);
                  addToken(text, i, i, Token.SEPARATOR, newStartOffset + i);
                  currentTokenType = Token.NULL;
                  break;
                }
              } else {
                if (RSyntaxUtilities.isLetterOrDigit(c) || c == '_') {
                  break;
                }
                int indexOf = separators.indexOf(c);
                if (indexOf > -1) {
                  addToken(text, currentTokenStart, i - 1, Token.IDENTIFIER,
                      newStartOffset + currentTokenStart);
                  addToken(text, i, i, Token.SEPARATOR, newStartOffset + i);
                  currentTokenType = Token.NULL;
                  break;
                }
              }
            }
          }
        }
        break;
        case Token.LITERAL_NUMBER_DECIMAL_INT: {
          switch (c) {
            case ' ':
            case '\t': {
              addToken(text, currentTokenStart, i - 1, Token.LITERAL_NUMBER_DECIMAL_INT,
                  newStartOffset + currentTokenStart);
              currentTokenStart = i;
              currentTokenType = Token.WHITESPACE;
            }
            break;
            case '\'': {
              addToken(text, currentTokenStart, i - 1, Token.LITERAL_NUMBER_DECIMAL_INT,
                  newStartOffset + currentTokenStart);
              currentTokenStart = i;
              currentTokenType = Token.LITERAL_STRING_DOUBLE_QUOTE;
              backslash = false;
            }
            break;
            case '_': {
              addToken(text, currentTokenStart, i - 1, Token.LITERAL_NUMBER_DECIMAL_INT,
                  newStartOffset + currentTokenStart);
              currentTokenStart = i;
              currentTokenType = Token.VARIABLE;
              backslash = false;
            }
            break;
            case '\\': {
              addToken(text, currentTokenStart, i - 1, Token.LITERAL_NUMBER_DECIMAL_INT,
                  newStartOffset + currentTokenStart);
              addToken(text, i, i, Token.IDENTIFIER, newStartOffset + i);
              currentTokenType = Token.NULL;
              backslash = true;
            }
            break;
            default: {
              if (RSyntaxUtilities.isDigit(c)) {
                break;
              }
              int indexOf = separators.indexOf(c);
              if (indexOf > -1) {
                addToken(text, currentTokenStart, i - 1, Token.LITERAL_NUMBER_DECIMAL_INT,
                    newStartOffset + currentTokenStart);
                addToken(text, i, i, Token.SEPARATOR, newStartOffset + i);
                currentTokenType = Token.NULL;
                break;
              }
              addToken(text, currentTokenStart, i - 1, Token.LITERAL_NUMBER_DECIMAL_INT,
                  newStartOffset + currentTokenStart);
              i--;
              currentTokenType = Token.NULL;
            }
          }
        }
        break;
        case Token.VARIABLE: {
          while (i < end) {
            c = array[i];
            if (!RSyntaxUtilities.isLetterOrDigit(c) && c != '_' || separators.indexOf(c) >= 0) {
              addToken(text, currentTokenStart, i - 1, Token.VARIABLE,
                  newStartOffset + currentTokenStart);
              i--;
              currentTokenType = Token.NULL;
              break;
            }
            i++;
          }
          if (i == end) {
            addToken(text, currentTokenStart, i - 1, Token.VARIABLE,
                newStartOffset + currentTokenStart);
            currentTokenType = Token.NULL;
          }
        }
        break;
        case Token.COMMENT_EOL: {
          i = end - 1;
          addToken(text, currentTokenStart, i, currentTokenType,
              newStartOffset + currentTokenStart);
          currentTokenType = Token.NULL;
        }
        break;
        case Token.LITERAL_CHAR: {
          if (c == '\\') {
            backslash = !backslash;
          } else {
            if (c == '\'' && !backslash) {
              addToken(text, currentTokenStart, i, Token.LITERAL_CHAR,
                  newStartOffset + currentTokenStart);
              currentTokenStart = i + 1;
              currentTokenType = Token.NULL;
            }
            backslash = false;
          }
        }
        break;
        case Token.LITERAL_STRING_DOUBLE_QUOTE: {
          switch (c) {
            case '\\': {
              backslash = !backslash;
            }
            break;
            case '\'': {
              if (!backslash) {
                addToken(text, currentTokenStart, i, Token.LITERAL_STRING_DOUBLE_QUOTE,
                    newStartOffset + currentTokenStart);
                currentTokenType = Token.NULL;
                break;
              }
              backslash = false;
            }
            break;
            default: {
              backslash = false;
            }
          }
        }
        break;
        default: {
          throw new Error("Must not be called");
        }
      }
    }

    switch (currentTokenType) {
      case Token.LITERAL_STRING_DOUBLE_QUOTE:
      case Token.LITERAL_CHAR: {
        addToken(text, currentTokenStart, end - 1, currentTokenType,
            newStartOffset + currentTokenStart);
      }
      break;
      case Token.NULL: {
        addNullToken();
      }
      break;
      default: {
        addToken(text, currentTokenStart, end - 1, currentTokenType,
            newStartOffset + currentTokenStart);
        addNullToken();
      }
    }
    return firstToken;
  }
}

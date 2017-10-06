package com.igormaznitsa.prol.easygui.tokenizer;

import javax.swing.text.Segment;
import org.fife.ui.rsyntaxtextarea.AbstractTokenMaker;
import org.fife.ui.rsyntaxtextarea.RSyntaxUtilities;
import org.fife.ui.rsyntaxtextarea.Token;
import org.fife.ui.rsyntaxtextarea.TokenMap;

public class JProlTokenMaker extends AbstractTokenMaker {

  protected final String operators = ",.;!|^/";
  protected final String separators = "()[]";
  protected final String separators2 = "+-^"; // Characters you don't want syntax highlighted but separate identifiers.

  private int currentTokenStart;
  private int currentTokenType;

  public JProlTokenMaker() {
    super();
  }

  /**
   * Checks the token to give it the exact ID it deserves before being passed up
   * to the super method.
   *
   * @param segment <code>Segment</code> to get text from.
   * @param start Start offset in <code>segment</code> of token.
   * @param end End offset in <code>segment</code> of token.
   * @param tokenType The token's type.
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
    return new String[]{"%", null};
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
    tokenMap.put(":-", reservedWord);
    tokenMap.put("?-", reservedWord);
    tokenMap.put("fail", reservedWord);
    tokenMap.put("::", reservedWord);
    tokenMap.put("div", reservedWord);
    tokenMap.put("mod", reservedWord);
    tokenMap.put("quot", reservedWord);
    tokenMap.put("rem", reservedWord);
    tokenMap.put("^", reservedWord);

    int function = Token.FUNCTION;
    tokenMap.put("op", function);

    int operator = Token.OPERATOR;
    tokenMap.put("=", operator);
    tokenMap.put("<", operator);
    tokenMap.put(">", operator);
    tokenMap.put("<>", operator);
    tokenMap.put("><", operator);
    tokenMap.put("<=", operator);
    tokenMap.put(">=", operator);
    tokenMap.put(":=", operator);
    tokenMap.put("==", operator);

    return tokenMap;

  }

  /**
   * Returns a list of tokens representing the given text.
   *
   * @param text The text to break into tokens.
   * @param startTokenType The token with which to start tokenizing.
   * @param startOffset The offset at which the line of tokens begins.
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
    // 'newStartOffset+currentTokenStart' for one less subraction operation.
    int newStartOffset = startOffset - offset;

    currentTokenStart = offset;
    currentTokenType = startTokenType;
    boolean backslash = false;

//beginning:
    for (int i = offset; i < end; i++) {

      char c = array[i];

      switch (currentTokenType) {

        case Token.NULL:

          currentTokenStart = i;	// Starting a new token here.

          switch (c) {

            case ' ':
            case '\t':
              currentTokenType = Token.WHITESPACE;
              break;

            case '\'':
              if (backslash) { // Escaped double quote => call '"' an identifier..
                addToken(text, currentTokenStart, i, Token.IDENTIFIER, newStartOffset + currentTokenStart);
                backslash = false;
              } else {
                currentTokenType = Token.LITERAL_STRING_DOUBLE_QUOTE;
              }
              break;

            case '\\':
              addToken(text, currentTokenStart, i, Token.IDENTIFIER, newStartOffset + currentTokenStart);
              currentTokenType = Token.NULL;
              backslash = !backslash;
              break;

            case '_':
              if (backslash) { // Escaped dollar sign => call '$' an identifier..
                addToken(text, currentTokenStart, i, Token.IDENTIFIER, newStartOffset + currentTokenStart);
                backslash = false;
              } else {
                currentTokenType = Token.VARIABLE;
              }
              break;

            case '%':
              backslash = false;
              currentTokenType = Token.COMMENT_EOL;
              break;

            default:
              if (RSyntaxUtilities.isLetter(c) && Character.isUpperCase(c)) {
                currentTokenType = Token.VARIABLE;
                break;
              }
              if (RSyntaxUtilities.isDigit(c)) {
                currentTokenType = Token.LITERAL_NUMBER_DECIMAL_INT;
                break;
              } else if (RSyntaxUtilities.isLetter(c) || c == '/' || c == '_') {
                currentTokenType = Token.IDENTIFIER;
                break;
              }
              int indexOf = operators.indexOf(c, 0);
              if (indexOf > -1) {
                addToken(text, currentTokenStart, i, Token.OPERATOR, newStartOffset + currentTokenStart);
                currentTokenType = Token.NULL;
                break;
              }
              indexOf = separators.indexOf(c, 0);
              if (indexOf > -1) {
                addToken(text, currentTokenStart, i, Token.SEPARATOR, newStartOffset + currentTokenStart);
                currentTokenType = Token.NULL;
                break;
              }
              indexOf = separators2.indexOf(c, 0);
              if (indexOf > -1) {
                addToken(text, currentTokenStart, i, Token.IDENTIFIER, newStartOffset + currentTokenStart);
                currentTokenType = Token.NULL;
                break;
              } else {
                currentTokenType = Token.IDENTIFIER;
                break;
              }

          } // End of switch (c).

          break;

        case Token.WHITESPACE:

          switch (c) {

            case ' ':
            case '\t':
              break;	// Still whitespace.

            case '\\':
              addToken(text, currentTokenStart, i - 1, Token.WHITESPACE, newStartOffset + currentTokenStart);
              addToken(text, i, i, Token.IDENTIFIER, newStartOffset + i);
              currentTokenType = Token.NULL;
              backslash = true; // Previous char whitespace => this must be first backslash.
              break;

            case '\'': // Don't need to worry about backslashes as previous char is space.
              addToken(text, currentTokenStart, i - 1, Token.WHITESPACE, newStartOffset + currentTokenStart);
              currentTokenStart = i;
              currentTokenType = Token.LITERAL_STRING_DOUBLE_QUOTE;
              backslash = false;
              break;

            case '_': // Don't need to worry about backslashes as previous char is space.
              addToken(text, currentTokenStart, i - 1, Token.WHITESPACE, newStartOffset + currentTokenStart);
              currentTokenStart = i;
              currentTokenType = Token.VARIABLE;
              backslash = false;
              break;

            case '%':
              addToken(text, currentTokenStart, i - 1, Token.WHITESPACE, newStartOffset + currentTokenStart);
              currentTokenStart = i;
              currentTokenType = Token.COMMENT_EOL;
              break;

            default:	// Add the whitespace token and start anew.

              addToken(text, currentTokenStart, i - 1, Token.WHITESPACE, newStartOffset + currentTokenStart);
              currentTokenStart = i;

              if (RSyntaxUtilities.isLetter(c) && Character.isUpperCase(c)) {
                currentTokenType = Token.VARIABLE;
                break;
              }

              if (RSyntaxUtilities.isDigit(c)) {
                currentTokenType = Token.LITERAL_NUMBER_DECIMAL_INT;
                break;
              } else if (RSyntaxUtilities.isLetter(c) || c == '/' || c == '_') {
                currentTokenType = Token.IDENTIFIER;
                break;
              }
              int indexOf = operators.indexOf(c, 0);
              if (indexOf > -1) {
                addToken(text, i, i, Token.OPERATOR, newStartOffset + i);
                currentTokenType = Token.NULL;
                break;
              }
              indexOf = separators.indexOf(c, 0);
              if (indexOf > -1) {
                addToken(text, i, i, Token.SEPARATOR, newStartOffset + i);
                currentTokenType = Token.NULL;
                break;
              }
              indexOf = separators2.indexOf(c, 0);
              if (indexOf > -1) {
                addToken(text, i, i, Token.IDENTIFIER, newStartOffset + i);
                currentTokenType = Token.NULL;
                break;
              } else {
                currentTokenType = Token.IDENTIFIER;
              }

          } // End of switch (c).

          break;

        default: // Should never happen
        case Token.IDENTIFIER:

          switch (c) {

            case ' ':
            case '\t':
              addToken(text, currentTokenStart, i - 1, Token.IDENTIFIER, newStartOffset + currentTokenStart);
              currentTokenStart = i;
              currentTokenType = Token.WHITESPACE;
              break;

            case '/': // Special-case to colorize commands like "echo" in "/bin/echo"
              addToken(text, currentTokenStart, i, Token.IDENTIFIER, newStartOffset + currentTokenStart);
              currentTokenStart = i + 1;
              currentTokenType = Token.NULL;
              break;

            case '\'': // Don't need to worry about backslashes as previous char is non-backslash.
              addToken(text, currentTokenStart, i - 1, Token.IDENTIFIER, newStartOffset + currentTokenStart);
              currentTokenStart = i;
              currentTokenType = Token.LITERAL_STRING_DOUBLE_QUOTE;
              backslash = false;
              break;

            case '\\':
              addToken(text, currentTokenStart, i - 1, Token.IDENTIFIER, newStartOffset + currentTokenStart);
              addToken(text, i, i, Token.IDENTIFIER, newStartOffset + i);
              currentTokenType = Token.NULL;
              backslash = true;
              break;

            default:
              if (RSyntaxUtilities.isLetterOrDigit(c) || c == '/' || c == '_') {
                break;	// Still an identifier of some type.
              }
              int indexOf = operators.indexOf(c);
              if (indexOf > -1) {
                addToken(text, currentTokenStart, i - 1, Token.IDENTIFIER, newStartOffset + currentTokenStart);
                addToken(text, i, i, Token.OPERATOR, newStartOffset + i);
                currentTokenType = Token.NULL;
                break;
              }
              indexOf = separators.indexOf(c, 0);
              if (indexOf > -1) {
                addToken(text, currentTokenStart, i - 1, Token.IDENTIFIER, newStartOffset + currentTokenStart);
                addToken(text, i, i, Token.SEPARATOR, newStartOffset + i);
                currentTokenType = Token.NULL;
                break;
              }
              indexOf = separators2.indexOf(c, 0);
              if (indexOf > -1) {
                addToken(text, currentTokenStart, i - 1, Token.IDENTIFIER, newStartOffset + currentTokenStart);
                addToken(text, i, i, Token.IDENTIFIER, newStartOffset + i);
                currentTokenType = Token.NULL;
                break;
              }
            // Otherwise, we're still an identifier (?).

          } // End of switch (c).

          break;

        case Token.LITERAL_NUMBER_DECIMAL_INT:

          switch (c) {

            case ' ':
            case '\t':
              addToken(text, currentTokenStart, i - 1, Token.LITERAL_NUMBER_DECIMAL_INT, newStartOffset + currentTokenStart);
              currentTokenStart = i;
              currentTokenType = Token.WHITESPACE;
              break;

            case '\'': // Don't need to worry about backslashes as previous char is non-backslash.
              addToken(text, currentTokenStart, i - 1, Token.LITERAL_NUMBER_DECIMAL_INT, newStartOffset + currentTokenStart);
              currentTokenStart = i;
              currentTokenType = Token.LITERAL_STRING_DOUBLE_QUOTE;
              backslash = false;
              break;

            case '_': // Don't need to worry about backslashes as previous char is non-backslash.
              addToken(text, currentTokenStart, i - 1, Token.LITERAL_NUMBER_DECIMAL_INT, newStartOffset + currentTokenStart);
              currentTokenStart = i;
              currentTokenType = Token.VARIABLE;
              backslash = false;
              break;

            case '\\':
              addToken(text, currentTokenStart, i - 1, Token.LITERAL_NUMBER_DECIMAL_INT, newStartOffset + currentTokenStart);
              addToken(text, i, i, Token.IDENTIFIER, newStartOffset + i);
              currentTokenType = Token.NULL;
              backslash = true;
              break;

            default:

              if (RSyntaxUtilities.isDigit(c)) {
                break;	// Still a literal number.
              }
              int indexOf = operators.indexOf(c);
              if (indexOf > -1) {
                addToken(text, currentTokenStart, i - 1, Token.LITERAL_NUMBER_DECIMAL_INT, newStartOffset + currentTokenStart);
                addToken(text, i, i, Token.OPERATOR, newStartOffset + i);
                currentTokenType = Token.NULL;
                break;
              }
              indexOf = separators.indexOf(c);
              if (indexOf > -1) {
                addToken(text, currentTokenStart, i - 1, Token.LITERAL_NUMBER_DECIMAL_INT, newStartOffset + currentTokenStart);
                addToken(text, i, i, Token.SEPARATOR, newStartOffset + i);
                currentTokenType = Token.NULL;
                break;
              }
              indexOf = separators2.indexOf(c);
              if (indexOf > -1) {
                addToken(text, currentTokenStart, i - 1, Token.LITERAL_NUMBER_DECIMAL_INT, newStartOffset + currentTokenStart);
                addToken(text, i, i, Token.IDENTIFIER, newStartOffset + i);
                currentTokenType = Token.NULL;
                break;
              }

              // Otherwise, remember this was a number and start over.
              addToken(text, currentTokenStart, i - 1, Token.LITERAL_NUMBER_DECIMAL_INT, newStartOffset + currentTokenStart);
              i--;
              currentTokenType = Token.NULL;

          } // End of switch (c).

          break;

        case Token.VARIABLE:

          // If we didn't find the '{' character, find the end of the variable...
          while (i < end) {
            c = array[i];	// Not needed the first iteration, but can't think of a better way to do it...
            if (!RSyntaxUtilities.isLetterOrDigit(c) && c != '_') {
              addToken(text, currentTokenStart, i - 1, Token.VARIABLE, newStartOffset + currentTokenStart);
              i--;
              currentTokenType = Token.NULL;
              break;
            }
            i++;
          }

          // This only happens if we never found the end of the variable in the loop above.
          if (i == end) {
            addToken(text, currentTokenStart, i - 1, Token.VARIABLE, newStartOffset + currentTokenStart);
            currentTokenType = Token.NULL;
          }

          break;

        case Token.COMMENT_EOL:
          i = end - 1;
          addToken(text, currentTokenStart, i, currentTokenType, newStartOffset + currentTokenStart);
          // We need to set token type to null so at the bottom we don't add one more token.
          currentTokenType = Token.NULL;

          break;

        case Token.LITERAL_CHAR:

          if (c == '\\') {
            backslash = !backslash; // Okay because if we got in here, backslash was initially false.
          } else {
            if (c == '\'' && !backslash) {
              addToken(text, currentTokenStart, i, Token.LITERAL_CHAR, newStartOffset + currentTokenStart);
              currentTokenStart = i + 1;
              currentTokenType = Token.NULL;
              // backslash is definitely false when we leave.
            }

            backslash = false; // Need to set backslash to false here as a character was typed.

          }
          // Otherwise, we're still an unclosed char literal...

          break;

        case Token.LITERAL_STRING_DOUBLE_QUOTE:

          switch (c) {

            case '\\':
              backslash = !backslash;
              break;

            case '\'':
              if (!backslash) {
                addToken(text, currentTokenStart, i, Token.LITERAL_STRING_DOUBLE_QUOTE, newStartOffset + currentTokenStart);
                currentTokenType = Token.NULL;
                // backslash is definitely false when we leave.
                break;
              }
              backslash = false;
              break;

            // Otherwise, we're still in an unclosed string...
            default:
              backslash = false; // Need to set backslash to false here as a character was typed.

          } // End of switch (c).

          break;

      } // End of switch (currentTokenType).

    } // End of for (int i=offset; i<end; i++).

    switch (currentTokenType) {

      // Remember what token type to begin the next line with.
      case Token.LITERAL_STRING_DOUBLE_QUOTE:
      case Token.LITERAL_CHAR:
        addToken(text, currentTokenStart, end - 1, currentTokenType, newStartOffset + currentTokenStart);
        break;

      // Do nothing if everything was okay.
      case Token.NULL:
        addNullToken();
        break;

      // All other token types don't continue to the next line...
      default:
        addToken(text, currentTokenStart, end - 1, currentTokenType, newStartOffset + currentTokenStart);
        addNullToken();

    }

    // Return the first token in our linked list.
    return firstToken;

  }
}

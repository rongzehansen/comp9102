/***
 ***
 *** 	 Scanner.java 	                         
 ***
 */

package VC.Scanner;

import VC.ErrorReporter;

public final class Scanner {

  private SourceFile sourceFile;
  private boolean debug;

  private ErrorReporter errorReporter;
  private StringBuffer currentSpelling;
  private char currentChar;
  private SourcePosition sourcePos;

  private int currentLineNumber;
  private int currentColumnNumber;

  // =========================================================

  public Scanner(SourceFile source, ErrorReporter reporter) {
    sourceFile = source;
    errorReporter = reporter;
    currentChar = sourceFile.getNextChar();
    debug = false;

    // you may initialise your counters for line and column numbers here
    currentLineNumber = 1;
    currentColumnNumber = 1;
  }

  public void enableDebugging() {
    debug = true;
  }

  // accept gets the next character from the source program.

  private void accept() {
    if (currentChar != '\n' && currentChar != '\t') {
      currentSpelling.append(currentChar);
    }

    if (currentChar == '\n') {
      currentLineNumber++;
      currentColumnNumber = 1; // Reset column number at the start of a new line
    } else if (currentChar == '\t') {
      int tabSize = 8;
      int spacesToNextTabStop = tabSize - ((currentColumnNumber - 1) % tabSize);
      currentColumnNumber += spacesToNextTabStop; // Move to the next tab stop
    } else {
      currentColumnNumber++; // Increment for regular characters
    }

    currentChar = sourceFile.getNextChar();

    // you may save the lexeme of the current token incrementally here
    // you may also increment your line and column counters here
  }

  // inspectChar returns the n-th character after currentChar
  // in the input stream.
  //
  // If there are fewer than nthChar characters between currentChar
  // and the end of file marker, SourceFile.eof is returned.
  //
  // Both currentChar and the current position in the input stream
  // are *not* changed. Therefore, a subsequent call to accept()
  // will always return the next char after currentChar.

  private char inspectChar(int nthChar) {
    return sourceFile.inspectChar(nthChar);
  }

  private int nextToken() {
    // Tokens: separators, operators, literals, identifiers and keyworods
    char nextChar = inspectChar(1);
    
    switch (currentChar) {
      case SourceFile.eof:
        currentSpelling.append(Token.spell(Token.EOF));
        return Token.EOF;
      // operators
      case '+':
        accept();
        return Token.PLUS;
      case '-':
        accept();
        return Token.MINUS;
      case '*':
        accept();
        return Token.MULT;
      case '/':
        accept();
        return Token.DIV;
      case '!':
        accept();
        return (nextChar == '=') ? acceptAndReturn(Token.NOTEQ) : Token.NOT;
      case '=':
        accept();
        return (nextChar == '=') ? acceptAndReturn(Token.EQEQ) : Token.EQ;
      case '<':
        accept();
        return (nextChar == '=') ? acceptAndReturn(Token.LTEQ) : Token.LT;
      case '>':
        accept();
        return (nextChar == '=') ? acceptAndReturn(Token.GTEQ) : Token.GT;
      case '&':
        accept();
        return (nextChar == '&') ? acceptAndReturn(Token.ANDAND) : Token.ERROR;
      case '|':
        accept();
        return (nextChar == '|') ? acceptAndReturn(Token.OROR) : Token.ERROR;
      // separators
      case '{':
        accept();
        return Token.LCURLY;
      case '}':
        accept();
        return Token.RCURLY;
      case '(':
        accept();
        return Token.LPAREN;
      case ')':
        accept();
        return Token.RPAREN;
      case '[':
        accept();
        return Token.LBRACKET;
      case ']':
        accept();
        return Token.RBRACKET;
      case ';':
        accept();
        return Token.SEMICOLON;
      case ',':
        accept();
        return Token.COMMA;
      case '"':
        sourcePos.lineStart = currentLineNumber;
        sourcePos.lineFinish = currentLineNumber;
        sourcePos.charStart = currentColumnNumber;
        accept();
        return handleStringLiteral();
      default:
        break;
    }
    
    if (Character.isDigit(currentChar) || (currentChar == '.' && Character.isDigit(inspectChar(1)))) {
      return handleNumericLiteral();
    }
    
    if (Character.isLetter(currentChar) || currentChar == '_') {
      return handleIdentifierOrKeyword();
    }

    accept();
    return Token.ERROR;
  }

  private int acceptAndReturn(int tokenKind) {
    accept();
    return tokenKind;
  }
  
  private int handleNumericLiteral() {
    boolean hasFraction = false;
    boolean hasExponent = false;
    
    while (Character.isDigit(currentChar)) {
      accept();
    }
    
    if (currentChar == '.') {
      hasFraction = true;
      accept();
      while (Character.isDigit(currentChar)) {
        accept();
      }
    }
    
    if (currentChar == 'e' || currentChar == 'E') {
      if (Character.isDigit(inspectChar(1))) {
        hasExponent = true;
        accept();
        while (Character.isDigit(currentChar)) {
          accept();
        }
      } else if ((inspectChar(1) == '+' || inspectChar(1) == '-') && Character.isDigit(inspectChar(2))) {
        hasExponent = true;
        accept();
        accept();
        while (Character.isDigit(currentChar)) {
          accept();
        }
      } 
    }
    if (hasFraction || hasExponent) {
      return Token.FLOATLITERAL;
    } else {
      return Token.INTLITERAL;
    }
  }
  
  private int handleIdentifierOrKeyword() {
    while (Character.isLetter(currentChar) || Character.isDigit(currentChar) || currentChar == '_') {
      accept();
    }
    
    switch (currentSpelling.toString()) {
      case "boolean":
        return Token.BOOLEAN;
      case "break":
        return Token.BREAK;
      case "continue":
        return Token.CONTINUE;
      case "else":
        return Token.ELSE;
      case "float":
        return Token.FLOAT;
      case "for":
        return Token.FOR;
      case "if":
        return Token.IF;
      case "int":
        return Token.INT;
      case "return":
        return Token.RETURN;
      case "void":
        return Token.VOID;
      case "while":
        return Token.WHILE;
      case "true":
        return Token.BOOLEANLITERAL;
      case "false":
        return Token.BOOLEANLITERAL;
      default:
        break;
    }
    
    return Token.ID;
  }
  
  private int handleStringLiteral() {
    currentSpelling.setLength(0);
    boolean terminated = false;
    while (!terminated && currentChar != SourceFile.eof && currentChar != '\n') {
      if (currentChar == '"') {
        terminated = true;
        accept(); // Consume the closing quote
        currentSpelling.deleteCharAt(currentSpelling.length() - 1);
      } else if (currentChar == '\\') { // Handle escape sequences
        char nextChar = inspectChar(1);
        switch (nextChar) {
          case 'b':
            accept();
            accept();
            currentSpelling.delete(currentSpelling.length() - 2, currentSpelling.length());
            currentSpelling.append('\b');
            break;
          case 'f':
            accept();
            accept();
            currentSpelling.delete(currentSpelling.length() - 2, currentSpelling.length());
            currentSpelling.append('\f');
            break;
          case 'n':
            accept();
            accept();
            currentSpelling.delete(currentSpelling.length() - 2, currentSpelling.length());
            currentSpelling.append('\n');
            break;
          case 'r':
            accept();
            accept();
            currentSpelling.delete(currentSpelling.length() - 2, currentSpelling.length());
            currentSpelling.append('\r');
            break;
          case 't':
            accept();
            accept();
            currentSpelling.delete(currentSpelling.length() - 2, currentSpelling.length());
            currentSpelling.append('\t');
            break;
          case '\'':
            accept();
            accept();
            currentSpelling.delete(currentSpelling.length() - 2, currentSpelling.length());
            currentSpelling.append('\'');
            break;
          case '\"':
            accept();
            accept();
            currentSpelling.delete(currentSpelling.length() - 2, currentSpelling.length());
            currentSpelling.append('\"');
            break;
          case '\\':
            accept();
            accept();
            currentSpelling.delete(currentSpelling.length() - 2, currentSpelling.length());
            currentSpelling.append('\\');
            break;
          default:
            // Report illegal escape character error
            sourcePos.charFinish = currentColumnNumber;
            accept();
            accept();
            errorReporter.reportError(String.format("\\%c: illegal escape character", nextChar), "",
                new SourcePosition(sourcePos.lineStart, sourcePos.charStart, sourcePos.charFinish));
            break;
        }
      } else {
        accept();
      }
    }

    if (!terminated) {
      errorReporter.reportError(String.format("%s: unterminated string", currentSpelling), "",
          new SourcePosition(sourcePos.lineStart, sourcePos.charStart, sourcePos.charStart));
    } 
    return Token.STRINGLITERAL;
  }

  void skipSpaceAndComments() {
    while (true) {
      while (Character.isWhitespace(currentChar))
        accept();
      if (currentChar == '/') {
        char nextChar = inspectChar(1);
        if (nextChar == '/') { // end-of-line comment
          accept(); // Consume '/'
          accept(); // Consume second '/'
          while (currentChar != '\n' && currentChar != SourceFile.eof) {
            accept();
          }
        } else if (nextChar == '*') { // traditional comment
          int commentStartLineNumber = currentLineNumber;
          int commentStartColumnNumber = currentColumnNumber;
          accept(); // Consume '/'
          accept(); // Consume '*'
          boolean commentClosed = false;
          while (!commentClosed && currentChar != SourceFile.eof) {
            if (currentChar == '*' && inspectChar(1) == '/') {
              accept(); // Consume '*'
              accept(); // Consume '/'
              commentClosed = true;
            } else {
              accept();
            }
          }
          if (!commentClosed) {
            errorReporter.reportError(": unterminated comment", "",
                new SourcePosition(commentStartLineNumber, commentStartColumnNumber, commentStartColumnNumber));
            return;
          }
        } else {
          break; // Not a comment
        }
      } else {
        break; // Not whitespace or comment start
      }
    }
  }

  public Token getToken() {
    Token tok;
    int kind;

    // skip white space and comments

    currentSpelling = new StringBuffer("");

    skipSpaceAndComments();

    currentSpelling.setLength(0);

    sourcePos = new SourcePosition();

    // You must record the position of the current token somehow
    sourcePos.lineStart = currentLineNumber;
    sourcePos.lineFinish = currentLineNumber;
    sourcePos.charStart = currentColumnNumber;

    kind = nextToken();

    if (currentColumnNumber > 1) {
      sourcePos.charFinish = currentColumnNumber - 1;
    } else {
      // If currentColumnNumber is 1, it means the token ends at the beginning of a
      // line
      // or is followed by an EOF, so set charFinish to also 1, ensuring it's never 0
      sourcePos.charFinish = 1;
    }

    tok = new Token(kind, currentSpelling.toString(), sourcePos);

    // * do not remove these three lines
    if (debug)
      System.out.println(tok);
    return tok;
  }

}

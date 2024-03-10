/*
 +--------------+
 + Parser.java  +
 +--------------+
 *
 * PLEASE COMPARE Recogniser.java PROVIDED IN ASSIGNMENT 2 AND Parser.java
 * PROVIDED BELOW TO UNDERSTAND HOW THE FORMER IS MODIFIED TO OBTAIN THE LATTER.
 *
 * This parser for a subset of the VC language is intended to 
 *  demonstrate how to create the AST nodes, including (among others): 
 *  (1) a list (of statements)
 *  (2) a function
 *  (3) a statement (which is an expression statement), 
 *  (4) a unary expression
 *  (5) a binary expression
 *  (6) terminals (identifiers, integer literals and operators)
 *
 * In addition, it also demonstrates how to use the two methods start 
 * and finish to determine the position information for the start and 
 * end of a construct (known as a phrase) corresponding an AST node.
 *
 * NOTE THAT THE POSITION INFORMATION WILL NOT BE MARKED. HOWEVER, IT CAN BE
 * USEFUL TO DEBUG YOUR IMPLEMENTATION.
 *
 *
 * --- 24/2/2024 --- 


program       -> func-decl
func-decl     -> type identifier "(" ")" compound-stmt
type          -> void
identifier    -> ID
// statements
compound-stmt -> "{" stmt* "}" 
stmt          -> expr-stmt
expr-stmt     -> expr? ";"
// expressions 
expr                -> additive-expr
additive-expr       -> multiplicative-expr
                    |  additive-expr "+" multiplicative-expr
                    |  additive-expr "-" multiplicative-expr
multiplicative-expr -> unary-expr
	            |  multiplicative-expr "*" unary-expr
	            |  multiplicative-expr "/" unary-expr
unary-expr          -> "-" unary-expr
		    |  primary-expr

primary-expr        -> identifier
 		    |  INTLITERAL
		    | "(" expr ")"
 */

package VC.Parser;

import VC.Scanner.Scanner;
import VC.Scanner.SourcePosition;
import VC.Scanner.Token;
import VC.ErrorReporter;
import VC.ASTs.*;

public class Parser {

  private Scanner scanner;
  private ErrorReporter errorReporter;
  private Token currentToken;
  private SourcePosition previousTokenPosition;
  private SourcePosition dummyPos = new SourcePosition();

  public Parser(Scanner lexer, ErrorReporter reporter) {
    scanner = lexer;
    errorReporter = reporter;

    previousTokenPosition = new SourcePosition();

    currentToken = scanner.getToken();
  }

  // match checks to see f the current token matches tokenExpected.
  // If so, fetches the next token.
  // If not, reports a syntactic error.

  void match(int tokenExpected) throws SyntaxError {
    if (currentToken.kind == tokenExpected) {
      previousTokenPosition = currentToken.position;
      currentToken = scanner.getToken();
    } else {
      syntacticError("\"%\" expected here", Token.spell(tokenExpected));
    }
  }

  void accept() {
    previousTokenPosition = currentToken.position;
    currentToken = scanner.getToken();
  }

  void syntacticError(String messageTemplate, String tokenQuoted) throws SyntaxError {
    SourcePosition pos = currentToken.position;
    errorReporter.reportError(messageTemplate, tokenQuoted, pos);
    throw (new SyntaxError());
  }

  // start records the position of the start of a phrase.
  // This is defined to be the position of the first
  // character of the first token of the phrase.

  void start(SourcePosition position) {
    position.lineStart = currentToken.position.lineStart;
    position.charStart = currentToken.position.charStart;
  }

  // finish records the position of the end of a phrase.
  // This is defined to be the position of the last
  // character of the last token of the phrase.

  void finish(SourcePosition position) {
    position.lineFinish = previousTokenPosition.lineFinish;
    position.charFinish = previousTokenPosition.charFinish;
  }

  void copyStart(SourcePosition from, SourcePosition to) {
    to.lineStart = from.lineStart;
    to.charStart = from.charStart;
  }

  // ========================== PROGRAMS ========================

  public Program parseProgram() {

    Program programAST = null;

    SourcePosition programPos = new SourcePosition();
    start(programPos);

    try {
      List dlAST = parseFuncOrVarDecl();
      finish(programPos);
      programAST = new Program(dlAST, programPos);
      if (currentToken.kind != Token.EOF) {
        syntacticError("\"%\" unknown type", currentToken.spelling);
      }
    } catch (SyntaxError s) {
      return null;
    }
    return programAST;
  }

  // ========================== DECLARATIONS ========================

  List parseFuncOrVarDecl() throws SyntaxError {
    SourcePosition funcOrVarPos = new SourcePosition();
    start(funcOrVarPos);

    List resultAST = null;
    List dlAST = null;
    Decl fAST = null;
    List vAST = null;
    Type tAST = null;
    Ident idAST = null;
    if (inFirstType(currentToken.kind)) {
      tAST = parseType();
      idAST = parseIdent();
      if (currentToken.kind == Token.LPAREN) {
        fAST = parsePartialFuncDecl(tAST, idAST);
      } else {
        vAST = parsePartialVarDecl(tAST, idAST);
      }
    } else {
      return new EmptyDeclList(dummyPos);
    }

    if (inFirstType(currentToken.kind)) {
      dlAST = parseFuncOrVarDecl();
    } else {
      dlAST = new EmptyDeclList(dummyPos);
    }

    if (fAST != null) {
      finish(funcOrVarPos);
      resultAST = new DeclList(fAST, dlAST, funcOrVarPos);
    }
    if (vAST != null) {
      DeclList rmDlAST = (DeclList) vAST;
      while(!(rmDlAST.DL instanceof EmptyDeclList)) {
        rmDlAST = (DeclList) rmDlAST.DL;
      }
      rmDlAST.DL = dlAST;
      resultAST = vAST;
    }
    return resultAST;
  }

  Decl parsePartialFuncDecl(Type tAST, Ident idAST) throws SyntaxError {
    SourcePosition funcPos = new SourcePosition();
    start(funcPos);
    List plAST = parseParaList();
    Stmt cAST = parseCompoundStmt();
    finish(funcPos);
    return new FuncDecl(tAST, idAST, plAST, cAST, funcPos);
  }

  List parsePartialVarDecl(Type tAST, Ident idAST) throws SyntaxError {
    // TODO
    SourcePosition varPos = new SourcePosition();
    start(varPos);

    List dlList = null;
    Type t = null;
    Decl dAST = null;
    if (currentToken.kind == Token.LBRACKET) {
      accept();
      Expr eAST = null;
      if (currentToken.kind == Token.INTLITERAL) {
        IntLiteral ilAST = parseIntLiteral();
        eAST = new IntExpr(ilAST, previousTokenPosition);
      } else {
        eAST = new EmptyExpr(dummyPos);
      }
      match(Token.RBRACKET);
      finish(varPos);
      t = new ArrayType(tAST, eAST, varPos);
    } else {
      finish(varPos);
      t = tAST;
    }

    SourcePosition initPos = new SourcePosition();
    copyStart(varPos, initPos);
    if (currentToken.kind == Token.EQ) {
      accept();
      Expr iAST = parseInitialiser();
      finish(initPos);
      dAST = new GlobalVarDecl(t, idAST, iAST, initPos);
    } else {
      finish(initPos);
      dAST = new GlobalVarDecl(t, idAST, new EmptyExpr(dummyPos), initPos);
    }

    if (currentToken.kind == Token.COMMA) {
      accept();
      List idlList = parseInitDeclaratorList(tAST, true);
      finish(initPos);
      dlList = new DeclList(dAST, idlList, initPos);
    } else {
      finish(initPos);
      dlList = new DeclList(dAST, new EmptyDeclList(dummyPos), initPos);
    }
    match(Token.SEMICOLON);

    return dlList;
  }

  Decl parseFuncDecl() throws SyntaxError {

    Decl fAST = null;

    SourcePosition funcPos = new SourcePosition();
    start(funcPos);

    Type tAST = parseType();
    Ident iAST = parseIdent();
    List fplAST = parseParaList();
    Stmt cAST = parseCompoundStmt();
    finish(funcPos);
    fAST = new FuncDecl(tAST, iAST, fplAST, cAST, funcPos);
    return fAST;
  }

  List parseVarDecl() throws SyntaxError {
    Type tAST = parseType();
    List dlList = parseInitDeclaratorList(tAST, false);
    match(Token.SEMICOLON);
    return dlList;
  }

  List parseInitDeclaratorList(Type tAST, boolean isGlobalVarDecl) throws SyntaxError {

    SourcePosition initPos = new SourcePosition();
    start(initPos);

    Decl dAST = parseInitDeclarator(tAST, isGlobalVarDecl);
    List dlList = null;
    if (currentToken.kind == Token.COMMA) {
      accept();
      dlList = parseInitDeclaratorList(tAST, isGlobalVarDecl);
      finish(initPos);
      return new DeclList(dAST, dlList, initPos);
    } else {
      finish(initPos);
      return new DeclList(dAST, new EmptyDeclList(dummyPos), initPos);
    }
  }

  Decl parseInitDeclarator(Type tAST, boolean isGlobalVarDecl) throws SyntaxError {
    // Decl dAST = null;

    SourcePosition initPos = new SourcePosition();
    start(initPos);

    Decl dAST = parseDeclarator(tAST);
    if (currentToken.kind == Token.EQ) {
      accept();
      Expr iAST = parseInitialiser();
      finish(initPos);
      if (isGlobalVarDecl) {
        return new GlobalVarDecl(dAST.T, dAST.I, iAST, initPos);
      } else {
        return new LocalVarDecl(dAST.T, dAST.I, iAST, initPos);
      }
    } else {
      finish(initPos);
      if (isGlobalVarDecl) {
        return new GlobalVarDecl(dAST.T, dAST.I, new EmptyExpr(dummyPos), initPos);
      } else {
        return new LocalVarDecl(dAST.T, dAST.I, new EmptyExpr(dummyPos), initPos);
      }
    }
  }

  Decl parseDeclarator(Type tAST) throws SyntaxError {
    // TODO
    SourcePosition declPos = new SourcePosition();
    start(declPos);

    Ident idAST = parseIdent();
    Type t = null;
    if (currentToken.kind == Token.LBRACKET) {
      accept();
      Expr eAST = null;
      if (currentToken.kind == Token.INTLITERAL) {
        IntLiteral ilAST = parseIntLiteral();
        eAST = new IntExpr(ilAST, previousTokenPosition);
      } else {
        eAST = new EmptyExpr(dummyPos);
      }
      match(Token.RBRACKET);
      finish(declPos);
      t = new ArrayType(tAST, eAST, declPos);
    } else {
      finish(declPos);
      t = tAST;
    }

    Decl dAST = new Decl(declPos) {

      @Override
      public Object visit(Visitor v, Object o) {
        return null;
      }

    };

    dAST.I = idAST;
    dAST.T = t;
    return dAST;
  }

  Expr parseInitialiser() throws SyntaxError {
    Expr iAST = null;

    SourcePosition initPos = new SourcePosition();
    start(initPos);

    if (currentToken.kind == Token.LCURLY) {
      accept();
      List ilList = parseInitExprList();
      finish(initPos);
      iAST = new ArrayInitExpr(ilList, initPos);
      match(Token.RCURLY);
    } else {
      iAST = parseExpr();
      finish(initPos);
    }
    
    return iAST;
  }

  List parseVarDeclList() throws SyntaxError {

    SourcePosition varPos = new SourcePosition();
    start(varPos);

    List dlAST = null;
    if (inFirstType(currentToken.kind)) {
      dlAST = parseVarDecl();
      DeclList rmDlAST = (DeclList) dlAST;
      while (!(rmDlAST.DL instanceof EmptyDeclList)) {
        rmDlAST = (DeclList) rmDlAST.DL;
      }
      rmDlAST.DL = parseVarDeclList();
    } else {
      finish(varPos);
      dlAST = new EmptyDeclList(dummyPos);
    }
    return dlAST;
  }
 
  List parseInitExprList() throws SyntaxError {
    SourcePosition initPos = new SourcePosition();
    start(initPos);

    Expr eAST = parseExpr();
    List elAST = null;
    if (currentToken.kind == Token.COMMA) {
      accept();
      elAST = parseInitExprList();
    } else {
      elAST = new EmptyArrayExprList(dummyPos);
    }
    finish(initPos);

    return new ArrayExprList(eAST, elAST, initPos);
  }

  // ======================== TYPES ==========================

  Type parseType() throws SyntaxError {
    Type typeAST = null;

    SourcePosition typePos = new SourcePosition();
    start(typePos);

    switch (currentToken.kind) {
      case Token.BOOLEAN:
        accept();
        finish(typePos);
        typeAST = new BooleanType(typePos);
        break;
      case Token.FLOAT:
        accept();
        finish(typePos);
        typeAST = new FloatType(typePos);
        break;
      case Token.INT:
        accept();
        finish(typePos);
        typeAST = new IntType(typePos);
        break;
      case Token.VOID:
        accept();
        finish(typePos);
        typeAST = new VoidType(typePos);
        break;
      default:
        syntacticError("\"%\" wrong result type for a function", currentToken.spelling);
        break;
    }

    return typeAST;
  }

  // ======================= STATEMENTS ==============================

  Stmt parseCompoundStmt() throws SyntaxError {
    Stmt cAST = null;

    SourcePosition stmtPos = new SourcePosition();
    start(stmtPos);

    match(Token.LCURLY);

    // Insert code here to build a DeclList node for variable declarations
    List dlAlST = parseVarDeclList();
    List slAST = parseStmtList();
    match(Token.RCURLY);
    finish(stmtPos);

    /*
     * In the subset of the VC grammar, no variable declarations are
     * allowed. Therefore, a block is empty iff it has no statements.
     */
    if (dlAlST instanceof EmptyDeclList && slAST instanceof EmptyStmtList)
      cAST = new EmptyCompStmt(stmtPos);
    else
      cAST = new CompoundStmt(dlAlST, slAST, stmtPos);
    return cAST;
  }

  List parseStmtList() throws SyntaxError {
    List slAST = null;

    SourcePosition stmtPos = new SourcePosition();
    start(stmtPos);

    if (currentToken.kind != Token.RCURLY) {
      Stmt sAST = parseStmt();
      {
        if (currentToken.kind != Token.RCURLY) {
          slAST = parseStmtList();
          finish(stmtPos);
          slAST = new StmtList(sAST, slAST, stmtPos);
        } else {
          finish(stmtPos);
          slAST = new StmtList(sAST, new EmptyStmtList(dummyPos), stmtPos);
        }
      }
    } else
      slAST = new EmptyStmtList(dummyPos);

    return slAST;
  }

  Stmt parseStmt() throws SyntaxError {
    Stmt sAST = null;

    switch (currentToken.kind) {
      case Token.LCURLY:
        sAST = parseCompoundStmt();
        break;
      case Token.IF:
        sAST = parseIfStmt();
        break;
      case Token.FOR:
        sAST = parseForStmt();
        break;
      case Token.WHILE:
        sAST = parseWhileStmt();
        break;
      case Token.BREAK:
        sAST = parseBreakStmt();
        break;
      case Token.CONTINUE:
        sAST = parseContinueStmt();
        break;
      case Token.RETURN:
        sAST = parseReturnStmt();
        break;
      default:
        sAST = parseExprStmt();
        break;
    }

    return sAST;
  }

  Stmt parseIfStmt() throws SyntaxError {
    Stmt sAST = null;

    SourcePosition stmtPos = new SourcePosition();
    start(stmtPos);

    match(Token.IF);
    match(Token.LPAREN);
    Expr eAST = parseExpr();
    match(Token.RPAREN);
    Stmt s1AST = parseStmt();
    Stmt s2AST = null;
    if (currentToken.kind == Token.ELSE) {
      accept();
      s2AST = parseStmt();
    }
    finish(stmtPos);
    if (s2AST == null) {
      sAST = new IfStmt(eAST, s1AST, stmtPos);
    } else {
      sAST = new IfStmt(eAST, s1AST, s2AST, stmtPos);
    }
    
    return sAST;
  }

  Stmt parseForStmt() throws SyntaxError {
    // Stmt sAST = null;

    SourcePosition stmtPos = new SourcePosition();
    start(stmtPos);

    match(Token.FOR);
    match(Token.LPAREN);
    Expr e1AST = null;
    if (inFirstExpr(currentToken.kind)) {
      e1AST = parseExpr();
    } else {
      e1AST = new EmptyExpr(dummyPos);
    }
    match(Token.SEMICOLON);
    Expr e2AST = null;
    if (inFirstExpr(currentToken.kind)) {
      e2AST = parseExpr();
    } else {
      e2AST = new EmptyExpr(dummyPos);
    }
    match(Token.SEMICOLON);
    Expr e3AST = null;
    if (inFirstExpr(currentToken.kind)) {
      e3AST = parseExpr();
    } else {
      e3AST = new EmptyExpr(dummyPos);
    }
    match(Token.RPAREN);
    Stmt sAST = parseStmt();
    finish(stmtPos);

    return new ForStmt(e1AST, e2AST, e3AST, sAST, stmtPos);
  }

  Stmt parseWhileStmt() throws SyntaxError {

    SourcePosition stmtPos = new SourcePosition();
    start(stmtPos);

    match(Token.WHILE);
    match(Token.LPAREN);
    Expr eAST = parseExpr();
    match(Token.RPAREN);
    Stmt sAST = parseStmt();
    finish(stmtPos);

    return new WhileStmt(eAST, sAST, stmtPos);
  }

  Stmt parseBreakStmt() throws SyntaxError {

    SourcePosition stmtPos = new SourcePosition();
    start(stmtPos);

    match(Token.BREAK);
    match(Token.SEMICOLON);
    finish(stmtPos);

    return new BreakStmt(stmtPos);
  }

  Stmt parseContinueStmt() throws SyntaxError {

    SourcePosition stmtPos = new SourcePosition();
    start(stmtPos);

    match(Token.CONTINUE);
    match(Token.SEMICOLON);

    finish(stmtPos);

    return new ContinueStmt(stmtPos);
  }

  Stmt parseReturnStmt() throws SyntaxError {

    SourcePosition stmtPos = new SourcePosition();
    start(stmtPos);

    match(Token.RETURN);
    Expr eAST = null;
    if (inFirstExpr(currentToken.kind)) {
      eAST = parseExpr();
    } else {
      eAST = new EmptyExpr(dummyPos);
    }
    match(Token.SEMICOLON);
    finish(stmtPos);

    return new ReturnStmt(eAST, stmtPos);
  }

  Stmt parseExprStmt() throws SyntaxError {
    Stmt sAST = null;

    SourcePosition stmtPos = new SourcePosition();
    start(stmtPos);

    if (inFirstExpr(currentToken.kind)) {
      Expr eAST = parseExpr();
      match(Token.SEMICOLON);
      finish(stmtPos);
      sAST = new ExprStmt(eAST, stmtPos);
    } else {
      match(Token.SEMICOLON);
      finish(stmtPos);
      sAST = new ExprStmt(new EmptyExpr(dummyPos), stmtPos);
    }
    return sAST;
  }

  // ======================= PARAMETERS =======================

  List parseParaList() throws SyntaxError {
    List formalsAST = null;

    SourcePosition formalsPos = new SourcePosition();
    start(formalsPos);

    match(Token.LPAREN);
    if (inFirstType(currentToken.kind)) {
      formalsAST = parseProperParaList();
    }
    match(Token.RPAREN);
    finish(formalsPos);

    if (formalsAST == null) {
      formalsAST = new EmptyParaList(formalsPos);
    }

    return formalsAST;
  }

  List parseProperParaList() throws SyntaxError {
    // List formalsAST = null;

    SourcePosition formalsPos = new SourcePosition();
    start(formalsPos);

    ParaDecl pAST = parseParaDecl();
    List plList = null;
    if (currentToken.kind == Token.COMMA) {
      accept();
      plList = parseProperParaList();
    } else {
      plList = new EmptyParaList(dummyPos);
    }
    finish(formalsPos);
    // formalsAST = new ParaList(pAST, plList, formalsPos);

    return new ParaList(pAST, plList, formalsPos);
  }

  ParaDecl parseParaDecl() throws SyntaxError {
    // ParaDecl pAST = null;

    SourcePosition paraPos = new SourcePosition();
    start(paraPos);

    Type tAST = parseType();
    Decl dAST = parseDeclarator(tAST);

    finish(paraPos);

    return new ParaDecl(dAST.T, dAST.I, paraPos);
  }

  List parseArgList() throws SyntaxError {
    List formalsAST = null;

    SourcePosition formalsPos = new SourcePosition();
    start(formalsPos);

    match(Token.LPAREN);
    if (inFirstExpr(currentToken.kind)) {
      formalsAST = parseProperArgList();
    }
    match(Token.RPAREN);
    finish(formalsPos);

    if (formalsAST == null) {
      formalsAST = new EmptyArgList(formalsPos);
    }

    return formalsAST;
  }

  List parseProperArgList() throws SyntaxError {
    SourcePosition formalsPos = new SourcePosition();
    start(formalsPos);

    Arg aAST = parseArg();
    List alList = null;
    if (currentToken.kind == Token.COMMA) {
      accept();
      alList = parseProperArgList();
    } else {
      alList = new EmptyArgList(dummyPos);
    }
    finish(formalsPos);

    return new ArgList(aAST, alList, formalsPos);
  }

  Arg parseArg() throws SyntaxError {
    Expr eAST = null;

    SourcePosition argPos = new SourcePosition();
    start(argPos);

    eAST = parseExpr();

    finish(argPos);

    return new Arg(eAST, argPos);
  }

  // ======================= EXPRESSIONS ======================

  Expr parseExpr() throws SyntaxError {
    Expr exprAST = null;
    exprAST = parseAssignExpr();
    return exprAST;
  }

  Expr parseAssignExpr() throws SyntaxError {
    Expr exprAST = null;

    SourcePosition assignPos = new SourcePosition();
    start(assignPos);

    exprAST = parseCondOrExpr();
    if (currentToken.kind == Token.EQ) {
      accept();
      Expr e2AST = parseAssignExpr();
      finish(assignPos);
      exprAST = new AssignExpr(exprAST, e2AST, assignPos);
    }
    return exprAST;
  }

  Expr parseCondOrExpr() throws SyntaxError {
    Expr exprAST = null;

    SourcePosition condOrStartPos = new SourcePosition();
    start(condOrStartPos);

    exprAST = parseCondAndExpr();
    while (currentToken.kind == Token.OROR) {
      Operator opAST = acceptOperator();
      Expr e2AST = parseCondAndExpr();

      SourcePosition condOrPos = new SourcePosition();
      copyStart(condOrStartPos, condOrPos);
      finish(condOrPos);
      exprAST = new BinaryExpr(exprAST, opAST, e2AST, condOrPos);
    }
    return exprAST;
  }

  Expr parseCondAndExpr() throws SyntaxError {
    Expr exprAST = null;

    SourcePosition condAndStartPos = new SourcePosition();
    start(condAndStartPos);

    exprAST = parseEqualityExpr();
    while (currentToken.kind == Token.ANDAND) {
      Operator opAST = acceptOperator();
      Expr e2AST = parseEqualityExpr();

      SourcePosition condAndPos = new SourcePosition();
      copyStart(condAndStartPos, condAndPos);
      finish(condAndPos);
      exprAST = new BinaryExpr(exprAST, opAST, e2AST, condAndPos);
    }
    return exprAST;
  }

  Expr parseEqualityExpr() throws SyntaxError {
    Expr exprAST = null;

    SourcePosition equalityStartPos = new SourcePosition();
    start(equalityStartPos);

    exprAST = parseRelExpr();
    while (currentToken.kind == Token.EQEQ || currentToken.kind == Token.NOTEQ) {
      Operator opAST = acceptOperator();
      Expr e2AST = parseRelExpr();

      SourcePosition equalityPos = new SourcePosition();
      copyStart(equalityStartPos, equalityPos);
      finish(equalityPos);
      exprAST = new BinaryExpr(exprAST, opAST, e2AST, equalityPos);
    }
    return exprAST;
  }

  Expr parseRelExpr() throws SyntaxError {
    Expr exprAST = null;

    SourcePosition relStartPos = new SourcePosition();
    start(relStartPos);

    exprAST = parseAdditiveExpr();
    while (currentToken.kind == Token.LT || currentToken.kind == Token.LTEQ || currentToken.kind == Token.GT
        || currentToken.kind == Token.GTEQ) {
      Operator opAST = acceptOperator();
      Expr e2AST = parseAdditiveExpr();

      SourcePosition relPos = new SourcePosition();
      copyStart(relStartPos, relPos);
      finish(relPos);
      exprAST = new BinaryExpr(exprAST, opAST, e2AST, relPos);
    }
    return exprAST;
  }

  Expr parseAdditiveExpr() throws SyntaxError {
    Expr exprAST = null;

    SourcePosition addStartPos = new SourcePosition();
    start(addStartPos);

    exprAST = parseMultiplicativeExpr();
    while (currentToken.kind == Token.PLUS
        || currentToken.kind == Token.MINUS) {
      Operator opAST = acceptOperator();
      Expr e2AST = parseMultiplicativeExpr();

      SourcePosition addPos = new SourcePosition();
      copyStart(addStartPos, addPos);
      finish(addPos);
      exprAST = new BinaryExpr(exprAST, opAST, e2AST, addPos);
    }
    return exprAST;
  }

  Expr parseMultiplicativeExpr() throws SyntaxError {

    Expr exprAST = null;

    SourcePosition multStartPos = new SourcePosition();
    start(multStartPos);

    exprAST = parseUnaryExpr();
    while (currentToken.kind == Token.MULT
        || currentToken.kind == Token.DIV) {
      Operator opAST = acceptOperator();
      Expr e2AST = parseUnaryExpr();
      SourcePosition multPos = new SourcePosition();
      copyStart(multStartPos, multPos);
      finish(multPos);
      exprAST = new BinaryExpr(exprAST, opAST, e2AST, multPos);
    }
    return exprAST;
  }

  Expr parseUnaryExpr() throws SyntaxError {

    Expr exprAST = null;

    SourcePosition unaryPos = new SourcePosition();
    start(unaryPos);

    switch (currentToken.kind) {
      case Token.PLUS:
      case Token.MINUS:
      case Token.NOT:
        Operator opAST = acceptOperator();
        Expr e2AST = parseUnaryExpr();
        finish(unaryPos);
        exprAST = new UnaryExpr(opAST, e2AST, unaryPos);
        break;

      default:
        exprAST = parsePrimaryExpr();
        break;

    }
    return exprAST;
  }

  Expr parsePrimaryExpr() throws SyntaxError {

    Expr exprAST = null;

    SourcePosition primPos = new SourcePosition();
    start(primPos);

    switch (currentToken.kind) {

      case Token.ID:
        /*
         * Ident iAST = parseIdent();
         * finish(primPos);
         * Var simVAST = new SimpleVar(iAST, primPos);
         * exprAST = new VarExpr(simVAST, primPos);
         */
        Ident idAST = parseIdent();
        if (currentToken.kind == Token.LPAREN) {
          List alList = parseArgList();
          finish(primPos);
          exprAST = new CallExpr(idAST, alList, primPos);
        } else if (currentToken.kind == Token.LBRACKET) {
          // TODO
          Var simVAST = new SimpleVar(idAST, previousTokenPosition);
          accept();

          Expr eAST = parseExpr();
          match(Token.RBRACKET);
          finish(primPos);
          exprAST = new ArrayExpr(simVAST, eAST, primPos);
        } else {
          finish(primPos);
          Var simVAST = new SimpleVar(idAST, primPos);
          exprAST = new VarExpr(simVAST, primPos);
        }
        break;

      case Token.LPAREN:
        accept();
        exprAST = parseExpr();
        match(Token.RPAREN);
        break;

      case Token.INTLITERAL:
        IntLiteral ilAST = parseIntLiteral();
        finish(primPos);
        exprAST = new IntExpr(ilAST, primPos);
        break;

      case Token.FLOATLITERAL:
        FloatLiteral flAST = parseFloatLiteral();
        finish(primPos);
        exprAST = new FloatExpr(flAST, primPos);
        break;

      case Token.BOOLEANLITERAL:
        BooleanLiteral blAST = parseBooleanLiteral();
        finish(primPos);
        exprAST = new BooleanExpr(blAST, primPos);
        break;

      case Token.STRINGLITERAL:
        StringLiteral slAST = parseStringLiteral();
        finish(primPos);
        exprAST = new StringExpr(slAST, primPos);
        break;

      default:
        syntacticError("illegal primary expression", currentToken.spelling);

    }
    return exprAST;
  }

  // ========================== ID, OPERATOR and LITERALS ========================

  Ident parseIdent() throws SyntaxError {

    Ident I = null;

    if (currentToken.kind == Token.ID) {
      previousTokenPosition = currentToken.position;
      String spelling = currentToken.spelling;
      I = new Ident(spelling, previousTokenPosition);
      currentToken = scanner.getToken();
    } else
      syntacticError("identifier expected here", "");
    return I;
  }

  // acceptOperator parses an operator, and constructs a leaf AST for it

  Operator acceptOperator() throws SyntaxError {
    Operator O = null;

    previousTokenPosition = currentToken.position;
    String spelling = currentToken.spelling;
    O = new Operator(spelling, previousTokenPosition);
    currentToken = scanner.getToken();
    return O;
  }

  IntLiteral parseIntLiteral() throws SyntaxError {
    IntLiteral IL = null;

    if (currentToken.kind == Token.INTLITERAL) {
      String spelling = currentToken.spelling;
      accept();
      IL = new IntLiteral(spelling, previousTokenPosition);
    } else
      syntacticError("integer literal expected here", "");
    return IL;
  }

  FloatLiteral parseFloatLiteral() throws SyntaxError {
    FloatLiteral FL = null;

    if (currentToken.kind == Token.FLOATLITERAL) {
      String spelling = currentToken.spelling;
      accept();
      FL = new FloatLiteral(spelling, previousTokenPosition);
    } else
      syntacticError("float literal expected here", "");
    return FL;
  }

  BooleanLiteral parseBooleanLiteral() throws SyntaxError {
    BooleanLiteral BL = null;

    if (currentToken.kind == Token.BOOLEANLITERAL) {
      String spelling = currentToken.spelling;
      accept();
      BL = new BooleanLiteral(spelling, previousTokenPosition);
    } else
      syntacticError("boolean literal expected here", "");
    return BL;
  }

  StringLiteral parseStringLiteral() throws SyntaxError {
    StringLiteral SL = null;

    if (currentToken.kind == Token.STRINGLITERAL) {
      String spelling = currentToken.spelling;
      accept();
      SL = new StringLiteral(spelling, previousTokenPosition);
    } else
      syntacticError("string literal expected here", "");
    return SL;
  }

  boolean inFirstType(int kind) {
    return kind == Token.BOOLEAN || kind == Token.FLOAT || kind == Token.INT
        || kind == Token.VOID;
  }

  boolean inFirstExpr(int kind) {
    return kind == Token.PLUS || kind == Token.MINUS || kind == Token.NOT
        || kind == Token.LPAREN || kind == Token.ID || kind == Token.INTLITERAL
        || kind == Token.FLOATLITERAL || kind == Token.BOOLEANLITERAL
        || kind == Token.STRINGLITERAL;
  }

}

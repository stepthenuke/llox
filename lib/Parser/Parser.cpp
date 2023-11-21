#include "llox/Parser/Parser.h"

namespace llox {
Parser::Parser(Lexer &L, Sema &S)
   : Lex(L), Sem(S) {
   nextToken();
}

void Parser::nextToken() {
   Lex.getNextToken(CurTok);
}

bool Parser::peekToken(tok::TokenKind Kind, int n = 1) {
   Token PeekedToken = Lex.peek(n);
   if (PeekedToken.is(Kind))
      return false;
   return true;
}

bool Parser::consumeToken(tok::TokenKind Kind) {
   if (CurTok.is(Kind)) {
      nextToken();
      return false;
   }
   return true;
}

bool Parser::consumeToken(std::initializer_list<tok::TokenKind> &&Kinds) {
   if (CurTok.isOneOf(std::move(Kinds))) {
      nextToken();
      return false;
   }
   return true;
}

op::OperatorPrec Parser::getBinOperatorPrec() {
   return op::getBinaryPrec(CurTok.getKind());
} 

op::OperatorPrec Parser::getUnOperatorPrec() {
   return op::getUnaryPrec(CurTok.getKind());
}

CompilationUnitDecl *Parser::parse() {
   CompilationUnitDecl *CompUnit = nullptr;
   parseCompilationUnit(CompUnit);
   return CompUnit;
}

bool Parser::parseCompilationUnit(CompilationUnitDecl *&CompUnit) {
   CompUnit = Sem.actOnCompilationUnit("COMPUNIT");
   Sem.enterScope(CompUnit);
   
   StmtList Stmts;
   if (parseStmtList(Stmts))
      return true;

   Sem.actOnCompilationUnit(CompUnit, Stmts);
   Sem.leaveScope();
   return false;
}

bool Parser::validStmtDelimiter(StmtList &Stmts) {
   // If, While, For, Block, Function ends with }; other stmts with ;
   Stmt *S = Stmts.back();
   return isa<IfStmt>(S) || isa<WhileStmt>(S)  || isa<BlockStmt>(S) 
      || isa<FunctionDecl>(S) || consumeToken(tok::semicolon);
}

bool Parser::parseStmtList(StmtList &Stmts) {
   if (parseStmt(Stmts))
      return true;

   while (CurTok.isNot(tok::eof) && validStmtDelimiter(Stmts)) {
      if (parseStmt(Stmts))
         return true;
   }
   return false;
}

bool Parser::parseStmt(StmtList &Stmts) {
   if (!parseDecl(Stmts))
      return false;
   
   switch (CurTok.getKind()) {
   case tok::kw_if:
      return parseIfStmt(Stmts);
   case tok::kw_while:
      return parseWhileStmt(Stmts);
   case tok::kw_for:
      return parseForStmt(Stmts);
   case tok::kw_return:
      return parseReturnStmt(Stmts);
   case tok::l_brace:
      return parseBlock(Stmts);
   default:
      return parseExprStmt(Stmts);
   }
   return true;
}

bool Parser::parseIfStmt(StmtList &Stmts) {
   if (consumeToken(tok::kw_if))
      return true;

   if (consumeToken(tok::l_paren))
      return true;

   Expr *Cond = nullptr;
   if (parseExpr(Cond))
      return true;

   if (consumeToken(tok::r_paren))
      return true;

   StmtList IfElseStmts;
   if (parseBlock(IfElseStmts))
      return true;

   if (CurTok.is(tok::kw_else)) {
      nextToken(); // eat else
      if (parseBlock(IfElseStmts))
         return true;
   }

   Sem.actOnIfStmt(Stmts, Cond, IfElseStmts);
   return false;
}

bool Parser::parseWhileStmt(StmtList &Stmts) {
   if (consumeToken(tok::kw_while))
      return true;

   if (consumeToken(tok::l_paren))
      return true;

   Expr *Cond;
   if (parseExpr(Cond))
      return true;

   if (consumeToken(tok::r_paren))
      return true;
   
   StmtList WhileStmts;
   if (parseBlock(WhileStmts))
      return true;

   Sem.actOnWhileStmt(Stmts, Cond, WhileStmts);
   return false;
}

// TODO with whileStmt after IR gen for while will be ready
bool Parser::parseForStmt(StmtList &Stmts) {
   llvm_unreachable("not impl");
}

bool Parser::parseReturnStmt(StmtList &Stmts) {
   if (consumeToken(tok::kw_return))
      return true;

   Expr *E;
   if (parseExpr(E))
      return true;
   
   Sem.actOnReturnStmt(Stmts, E);
   return false;
}

bool Parser::parseBracedStmts(StmtList &Stmts) {
   if (consumeToken(tok::l_brace))
      return true;
   
   while (CurTok.isNot(tok::eof) && CurTok.isNot(tok::r_brace)) {
      if (parseStmt(Stmts))
         return true;
   }

   if (consumeToken(tok::r_brace))
      return true;

   return false;
}

bool Parser::parseBlock(StmtList &Stmts) {
   BlockStmt *Block = Sem.actOnBlockStmt(Stmts);
   Sem.enterScope(Block);

   StmtList BlockStmts;
   if (parseBracedStmts(BlockStmts))
      return true;

   Sem.actOnBlockStmt(Block, BlockStmts);
   Sem.leaveScope();
   return false;
}

bool Parser::parseExprStmt(StmtList &Stmts) {
   Expr *E = nullptr;
   parseExpr(E);
   if (consumeToken(tok::semicolon))
      return true;
   
   Sem.actOnExprStmt(Stmts, E);
   return false;
}

bool Parser::parseDecl(StmtList &Decls) {
   switch (CurTok.getKind()) {
   case tok::kw_var:
      return parseVariableDecl(Decls);
   case tok::kw_fun:
      return parseFunctionDecl(Decls);
   default:
      break;
   }
   return true;
}

bool Parser::parseVariableDecl(StmtList &Decls) {
   if (consumeToken(tok::kw_var))
      return true;
   if (CurTok.isNot(tok::identifier))
      return true;
   Identifier VarId {CurTok.getLocation(), CurTok.getIdentifier()};
   nextToken();

   if (consumeToken(tok::colon))
      return true;

   Decl *D;
   if (parseTypeIdent(D))
      return true;

   Sem.actOnVariableDecl(Decls, VarId, D);
   
   if (CurTok.is(tok::equal)) {
      nextToken();
      Expr *E;
      if (parseExpr(E))
         return true;
      Sem.actOnAssignmentInit(Decls, E);
   }

   if (consumeToken(tok::semicolon))
      return true;

   return false;
}

bool Parser::parseTypeIdent(Decl *&D) {
   D = nullptr;
   if (CurTok.is(tok::kw_nil))
      return false;
   if (CurTok.isNot(tok::identifier))
      return true;
   D = Sem.actOnNameLookup(D, CurTok.getLocation(),  CurTok.getIdentifier());
   nextToken();
   return false;
}

bool Parser::parseFunctionParameter(IdentList &ParIds, StmtList &ParTypes) {
   if (CurTok.isNot(tok::identifier))
      return true;
   ParIds.emplace_back(CurTok.getLocation(), CurTok.getIdentifier());
   nextToken();

   if (consumeToken(tok::colon))
      return true;

   Decl *D;
   if (parseTypeIdent(D))
      return true;
   ParTypes.push_back(D);

   return false;      
}

bool Parser::parseFunctionParameterList(ParameterList &Params) {
   if (CurTok.is(tok::r_paren)) {
      nextToken();
      return false;
   }

   if (CurTok.isNot(tok::identifier))
      return true;
   
   IdentList ParIds;
   StmtList ParTypes;
   if (parseFunctionParameter(ParIds, ParTypes))
      return true;
   while (CurTok.is(tok::comma)) {
      nextToken(); // eat comma
      if (parseFunctionParameter(ParIds, ParTypes))
         return true;
   }

   if (consumeToken(tok::r_paren))
      return true;

   Sem.actOnFunctionParameters(Params, ParIds, ParTypes);
   return false;
}

bool Parser::parseFunctionDecl(StmtList &Decls) {
   if (consumeToken(tok::kw_fun))
      return true;

   if (CurTok.isNot(tok::identifier))
      return true;
   FunctionDecl *FunDecl = Sem.actOnFunctionDecl(CurTok.getLocation(), CurTok.getIdentifier());
   Sem.enterScope(FunDecl);
   nextToken();

   // here we need to create function -> add it to scope
   // and enter its scope -> parse parameters and add them to funcs' scope
   
   if (consumeToken(tok::l_paren))
      return true;

   ParameterList FunParams;
   if (parseFunctionParameterList(FunParams))
      return true;

   if (consumeToken(tok::colon))
      return true;
   
   Decl *RetTy;
   if (parseTypeIdent(RetTy))
      return true;
      
   Sem.actOnFunctionParamList(FunDecl, FunParams, RetTy);

   StmtList FunStmts;
   if (parseBracedStmts(FunStmts))
      return true;
   
   Sem.actOnFunctionBlock(Decls, FunDecl, FunStmts);
   Sem.leaveScope();

   return false;
}

bool Parser::parseExprList(ExprList &Exprs) {
   Expr *E = nullptr;
   if (parseExpr(E))
      return true;
   if (E)
      Exprs.push_back(E);

   while (CurTok.is(tok::comma)) {
      nextToken();
      E = nullptr;
      if (parseExpr(E))
         return true;
      if (E)
         Exprs.push_back(E);
   }
   return false;
}

bool Parser::parseIdentifierExpr(Expr *&E) {
   if (CurTok.isNot(tok::identifier))
      return true;
   
   Identifier Id (CurTok.getLocation(), CurTok.getIdentifier());
   nextToken();
   if (CurTok.isNot(tok::l_paren)) {
      E = Sem.actOnObjectExpr(Id);
      return false;
   }
   nextToken();

   ExprList ParamExprs;
   if (parseExprList(ParamExprs))
      return true;
   
   if (consumeToken(tok::r_paren))
      return true;
   E = Sem.actOnFunctionCallExpr(Id, ParamExprs);
   return false;
}

bool Parser::parseStringLiteral(Expr *&E) {
   return true;
}

bool Parser::parseParenExpr(Expr *&E) {
   consumeToken(tok::l_paren);
   if (parseExpr(E))
      return true;
   consumeToken(tok::r_paren);
   return false;
}

bool Parser::parseDoubleLiteral(Expr *&E) {
   if (CurTok.is(tok::double_literal)) {
      E = Sem.actOnDoubleLiteral(CurTok.getLocation(), CurTok.getLiteral());
      assert(isa<DoubleLiteral>(E));
      nextToken();
      return false;
   }
   return true;
}

bool Parser::parseBoolLiteral(Expr *&E) {
   if (CurTok.isOneOf({tok::kw_false, tok::kw_true})) {
      E = Sem.actOnBoolLiteral(CurTok.getKind());
      nextToken();
      return false;
   }
   return true;
}

bool Parser::parsePrimary(Expr *&E) {
   switch (CurTok.getKind()) {
   case tok::kw_false:
   case tok::kw_true:
      return parseBoolLiteral(E);  // TODO: do smth on false and true
   case tok::identifier:
      return parseIdentifierExpr(E);
   case tok::double_literal:
      return parseDoubleLiteral(E);
   case tok::string_literal:
      return parseStringLiteral(E);
   case tok::l_paren:
      return parseParenExpr(E);
   default:
      return false;
   }
}

bool Parser::parseExpr(Expr *&E) {
   if (parsePrefixExpr(E))
      return true;
   if (!E)
      return false;
   if (CurTok.is(tok::r_paren))
      return false;

   return parseInfixExpr(op::Prec_None, E);
}

bool Parser::parseInfixExpr(OperatorPrec LeftPrec, Expr *&Left) {
   while (true) {
      if (CurTok.isOneOf({tok::comma, tok::semicolon, tok::r_paren})) {
         return false;
      }

      OperatorInfo BinOp(CurTok.getLocation(), CurTok.getKind());
      OperatorPrec BinOpPrec = BinOp.getBinPrec();

      if (BinOpPrec < LeftPrec)
         return false;

      nextToken(); // eat BinOp
      
      // there can be expr: 10 + -1 -> parsing + -> we find unary expr
      Expr *Right = nullptr;
      if (parsePrefixExpr(Right))
         return true;
      if (!Right)
         return false;

      OperatorPrec NextBinOpPrec = op::getBinaryPrec(CurTok.getKind());
      if (NextBinOpPrec > BinOpPrec) {
         OperatorPrec NewPrec = (op::Prec_Primary < static_cast<OperatorPrec>(BinOpPrec + 1)) 
                                 ? op::Prec_Primary : static_cast<OperatorPrec>(BinOpPrec + 1);
         if (parseInfixExpr(NewPrec, Right))
            return true;
         if (!Right)
            return false;
      }
      Left = Sem.actOnInfixExpr(Left, Right, BinOp);
   }
}

bool Parser::parsePrefixExpr(Expr *&E) {
   if (!op::isUnaryOp(CurTok.getKind()))
      return parsePrimary(E);

   OperatorInfo UnOp(CurTok.getLocation(), CurTok.getKind());
   nextToken();
   Expr *Operand = nullptr;
   if (parsePrefixExpr(Operand))
      return true;
   if (!Operand)
      return true;
   
   E = Sem.actOnPrefixExpr(Operand, UnOp);
   return false;
}

} // namespace llox

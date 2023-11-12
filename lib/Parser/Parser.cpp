#include "llox/Parser/Parser.h"

namespace llox {
   Parser::Parser(Lexer &L, Sema &S)
      : Lex(L), Sem(S) {
      nextToken();
   }

   void Parser::nextToken() {
      Lex.getNextToken(CurTok);
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

   OperatorPrec Parser::getBinOperatorPrec() {
      return op::getBinaryPrec(CurTok.getKind());
   } 

   OperatorPrec Parser::getUnOperatorPrec() {
      return op::getUnaryPrec(CurTok.getKind());
   }

   bool Parser::parseProgram();
   
   bool Parser::parseDecl(DeclList &Decls) {
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
   
   bool Parser::parseVariableDecl(DeclList &Decls) {
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

   bool Parser::parseFunctionParameter(IdentList &ParIds, DeclList &ParTypes) {
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
      DeclList ParTypes;
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

   bool Parser::parseBlock(DeclList &Decls, StmtList &Stmts) {
      if (consumeToken(tok::l_brace))
         return true;
      
      while (CurTok.isNot(tok::eof) && CurTok.isNot(tok::r_brace))
         nextToken();

      if (consumeToken(tok::r_brace))
         return true;

      return false;
   }

   bool Parser::parseFunctionDecl(DeclList &Decls) {
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

      DeclList FunDecls;
      StmtList FunStmts;
      if (parseBlock(FunDecls, FunStmts))
         return true;
      
      Sem.actOnFunctionBlock(Decls, FunDecl, FunDecls, FunStmts);
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
         E = Sem.actOnVariableExpr(Id);
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
      assert(E != nullptr);
      if (!E)
         return false;
      if (CurTok.is(tok::r_paren))
         return false;

      return parseInfixExpr(op::Prec_None, E);
   }
   
   bool Parser::parseInfixExpr(OperatorPrec LeftPrec, Expr *&Left) {
      while (true) {
         if (CurTok.is(tok::comma)) {
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
      if (!op::isUnaryOp(CurTok.getKind())) {
         return parsePrimary(E);
      }

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

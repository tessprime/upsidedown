// external_symbol_tracker.h
#pragma once

#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/Stmt.h"
#include "upsidedown_utils.h"
#include <map>
#include <set>
#include <string>

class ExternalSymbolTracker : public clang::RecursiveASTVisitor<ExternalSymbolTracker> {
public:
  ExternalSymbolTracker(const clang::SourceManager &SM)
      : SM(SM), CurrentFunction(nullptr) {}

  bool VisitFunctionDecl(clang::FunctionDecl *FD) {
    if (!FD || !FD->hasBody()) return true;
    if (!isUserLocation(SM, FD->getLocation())) return true;

    CurrentFunction = FD;
    return true;
  }

  bool VisitDeclRefExpr(clang::DeclRefExpr *E) {
    if (!E || !CurrentFunction) return true;

    const clang::ValueDecl *D = E->getDecl();
    if (!D) return true;

    if (const auto *FD = llvm::dyn_cast<clang::FunctionDecl>(D)) {
      if (FD->isOverloadedOperator()) return true;
    }

    clang::SourceLocation DeclLoc = D->getLocation();
    if (DeclLoc.isInvalid()) return true;
    DeclLoc = SM.getExpansionLoc(DeclLoc);

    if (!SM.isWrittenInMainFile(DeclLoc)) {
      std::string FullName;
      llvm::raw_string_ostream OS(FullName);
      E->getDecl()->printQualifiedName(OS);
      OS.flush();

      FunctionExternals[CurrentFunction].insert(FullName);
      ExternalDeclMap[FullName] = E->getDecl();
    }

    return true;
  }

  bool VisitMemberExpr(clang::MemberExpr *E) {
    if (!E || !CurrentFunction) return true;

    const clang::ValueDecl *D = E->getMemberDecl();
    if (!D) return true;

    clang::SourceLocation DeclLoc = D->getLocation();
    if (DeclLoc.isInvalid()) return true;
    DeclLoc = SM.getExpansionLoc(DeclLoc);

    if (!SM.isWrittenInMainFile(DeclLoc)) {
      std::string FullName;
      llvm::raw_string_ostream OS(FullName);
      D->printQualifiedName(OS);
      OS.flush();

      FunctionExternals[CurrentFunction].insert(FullName);
      ExternalDeclMap[FullName] = D;
    }

    return true;
  }

  std::map<const clang::FunctionDecl*, std::set<std::string>> FunctionExternals;
  std::map<std::string, const clang::NamedDecl*> ExternalDeclMap;

private:
  const clang::SourceManager &SM;
  clang::FunctionDecl *CurrentFunction;
};

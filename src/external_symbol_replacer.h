// external_symbol_replacer.h
#pragma once

#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Tooling/Core/Replacement.h"
#include "upsidedown_utils.h"
#include <map>
#include <string>

class ExternalSymbolReplacer : public clang::RecursiveASTVisitor<ExternalSymbolReplacer> {
public:
  ExternalSymbolReplacer(const clang::SourceManager &SM, clang::tooling::Replacements &Repls,
                         const std::map<const clang::FunctionDecl*, std::map<std::string, std::string>> &AliasMap)
      : SM(SM), Repls(Repls), AliasMap(AliasMap), CurrentFunction(nullptr) {}

  bool VisitFunctionDecl(clang::FunctionDecl *FD) {
    if (!FD || !FD->hasBody()) return true;
    CurrentFunction = FD;
    return true;
  }

  bool VisitDeclRefExpr(clang::DeclRefExpr *E) {
    if (!E || !CurrentFunction) return true;

    auto FuncIt = AliasMap.find(CurrentFunction);
    if (FuncIt == AliasMap.end()) return true;

    const clang::ValueDecl *D = E->getDecl();
    if (!D) return true;

    std::string FullName;
    llvm::raw_string_ostream OS(FullName);
    D->printQualifiedName(OS);
    OS.flush();

    auto AliasIt = FuncIt->second.find(FullName);
    if (AliasIt != FuncIt->second.end()) {
      clang::SourceLocation Loc = E->getLocation();
      if (Loc.isInvalid()) return true;
      Loc = SM.getExpansionLoc(Loc);

      if (!SM.isWrittenInMainFile(Loc)) return true;

      clang::SourceRange Range = E->getSourceRange();
      if (Range.isInvalid()) return true;

      auto StartLoc = Range.getBegin();
      auto EndLoc = Range.getEnd();

      EndLoc = clang::Lexer::getLocForEndOfToken(EndLoc, 0, SM, clang::LangOptions());

      unsigned Length = SM.getFileOffset(EndLoc) - SM.getFileOffset(StartLoc);

      auto Err = Repls.add(clang::tooling::Replacement(SM, Range.getBegin(), Length, AliasIt->second));
      if (Err) {
        llvm::consumeError(std::move(Err));
      }
    }

    return true;
  }

private:
  const clang::SourceManager &SM;
  clang::tooling::Replacements &Repls;
  const std::map<const clang::FunctionDecl*, std::map<std::string, std::string>> &AliasMap;
  clang::FunctionDecl *CurrentFunction;
};

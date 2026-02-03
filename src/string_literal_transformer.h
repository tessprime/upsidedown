// string_literal_transformer.h
#pragma once

#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/AST/ASTContext.h"
#include "clang/Lex/Lexer.h"
#include "clang/Tooling/Core/Replacement.h"
#include "upsidedown_utils.h"

class StringLiteralTransformer : public clang::RecursiveASTVisitor<StringLiteralTransformer> {
public:
  StringLiteralTransformer(const clang::SourceManager &SM, clang::tooling::Replacements &Repls)
      : SM(SM), Repls(Repls) {}

  bool VisitStringLiteral(clang::StringLiteral *SL) {
    if (!SL) return true;

    clang::SourceLocation Loc = SL->getBeginLoc();
    if (Loc.isInvalid()) return true;
    Loc = SM.getExpansionLoc(Loc);

    if (!SM.isWrittenInMainFile(Loc)) return true;

    std::string Interpreted = SL->getString().str();
    std::string Transformed = toUpsideDown(Interpreted, false);
    std::string Escaped = escapeString(Transformed);

    clang::SourceRange Range = SL->getSourceRange();
    clang::SourceLocation EndLoc = Range.getEnd();
    if (EndLoc.isInvalid()) return true;
    EndLoc = SM.getExpansionLoc(EndLoc);

    clang::SourceLocation AfterQuote = clang::Lexer::getLocForEndOfToken(EndLoc, 0, SM, clang::LangOptions());

    unsigned TotalLength = SM.getFileOffset(AfterQuote) - SM.getFileOffset(Loc);
    unsigned ContentLength = TotalLength - 2;

    clang::SourceLocation StartLoc = Loc.getLocWithOffset(1);

    auto Err = Repls.add(clang::tooling::Replacement(SM, StartLoc, ContentLength, Escaped));
    if (Err) {
      llvm::consumeError(std::move(Err));
    }

    return true;
  }

private:
  const clang::SourceManager &SM;
  clang::tooling::Replacements &Repls;
};

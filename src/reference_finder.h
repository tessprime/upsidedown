// reference_finder.h
#pragma once

#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/Stmt.h"
#include "clang/Tooling/Core/Replacement.h"
#include "clang/AST/ASTContext.h"
#include "upsidedown_utils.h"
#include <string>

class ReferenceFinder : public clang::RecursiveASTVisitor<ReferenceFinder> {
public:
  ReferenceFinder(const clang::NamedDecl *Target, const clang::SourceManager &SM,
                  clang::tooling::Replacements &Repls, const std::string &NewName,
                  const std::string &OldName)
      : Target(Target->getCanonicalDecl()), SM(SM), Repls(Repls),
        NewName(NewName), OldNameLength(OldName.length()) {}

  bool VisitDeclRefExpr(clang::DeclRefExpr *E) {
    const clang::ValueDecl *D = E->getDecl();
    if (!D) return true;
    if (D->getCanonicalDecl() == Target) {
      addReplacement(E->getLocation(), OldNameLength);
    }
    return true;
  }

  bool VisitMemberExpr(clang::MemberExpr *E) {
    const clang::ValueDecl *D = E->getMemberDecl();
    if (!D) return true;
    if (D->getCanonicalDecl() == Target) {
      addReplacement(E->getMemberLoc(), OldNameLength);
    }
    return true;
  }

  bool VisitTypeLoc(clang::TypeLoc TL) {
    if (auto ElabTL = TL.getAs<clang::ElaboratedTypeLoc>()) {
      TL = ElabTL.getNamedTypeLoc();
    }

    if (auto TagTL = TL.getAs<clang::TagTypeLoc>()) {
      const clang::TagDecl *TD = TagTL.getDecl();
      if (TD && TD->getCanonicalDecl() == Target) {
        addReplacement(TagTL.getNameLoc(), OldNameLength);
      }
    } else if (auto TDT = TL.getAs<clang::TypedefTypeLoc>()) {
      const clang::TypedefNameDecl *TD = TDT.getTypedefNameDecl();
      if (TD && TD->getCanonicalDecl() == Target) {
        addReplacement(TDT.getNameLoc(), OldNameLength);
      }
    }
    return true;
  }

  bool VisitNamedDecl(clang::NamedDecl *D) {
    if (D->getCanonicalDecl() == Target) {
      addReplacement(D->getLocation(), OldNameLength);
    }
    return true;
  }

private:
  void addReplacement(clang::SourceLocation Loc, unsigned Length) {
    if (Loc.isInvalid()) return;
    Loc = SM.getExpansionLoc(Loc);
    if (Loc.isInvalid()) return;

    if (!SM.isWrittenInMainFile(Loc)) return;

    auto Err = Repls.add(clang::tooling::Replacement(SM, Loc, Length, NewName));
    if (Err) {
      llvm::consumeError(std::move(Err));
    }
  }

  const clang::Decl *Target;
  const clang::SourceManager &SM;
  clang::tooling::Replacements &Repls;
  std::string NewName;
  unsigned OldNameLength;
};

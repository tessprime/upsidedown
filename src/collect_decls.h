// collect_decls.h
#pragma once

#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "upsidedown_utils.h"
#include <vector>

class CollectDecls : public clang::ast_matchers::MatchFinder::MatchCallback {
public:
  explicit CollectDecls(std::vector<const clang::NamedDecl*> &Out) : Out(Out) {}

  void run(const clang::ast_matchers::MatchFinder::MatchResult &Result) override {
    auto *D = Result.Nodes.getNodeAs<clang::NamedDecl>("nd");
    if (!D) return;
    if (!shouldRename(D, *Result.Context)) return;

    const clang::NamedDecl *Canon = llvm::dyn_cast<clang::NamedDecl>(D->getCanonicalDecl());
    if (!Canon) Canon = D;

    Out.push_back(Canon);
  }

private:
  std::vector<const clang::NamedDecl*> &Out;
};

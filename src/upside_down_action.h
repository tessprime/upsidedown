// upside_down_action.h
#pragma once

#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Frontend/CompilerInstance.h"
#include "llvm/Support/raw_ostream.h"
#include "clang/Tooling/Core/Replacement.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/Index/USRGeneration.h"
#include "clang/AST/ASTContext.h"
#include "collect_decls.h"
#include "reference_finder.h"
#include "string_literal_transformer.h"
#include "external_symbol_tracker.h"
#include "external_symbol_replacer.h"

#include <memory>
#include <map>
#include <vector>

class UpsideDownAction : public clang::ASTFrontendAction {
public:
  bool BeginSourceFileAction(clang::CompilerInstance &CI) override {
    Repls.clear();
    return true;
  }

  std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(clang::CompilerInstance &CI,
                                                 clang::StringRef InFile) override {
    Decls.clear();
    Finder = std::make_unique<clang::ast_matchers::MatchFinder>();
    Callback = std::make_unique<CollectDecls>(Decls);

    Finder->addMatcher(clang::ast_matchers::namedDecl().bind("nd"), Callback.get());

    return Finder->newASTConsumer();
  }

  void EndSourceFileAction() override {
    auto &CI = getCompilerInstance();
    clang::ASTContext &Ctx = CI.getASTContext();
    auto &SM = Ctx.getSourceManager();

    std::map<std::string, const clang::NamedDecl*> ByUSR;

    for (const clang::NamedDecl *D : Decls) {
      llvm::SmallString<128> USR;
      if (clang::index::generateUSRForDecl(D, USR)) continue;
      ByUSR[std::string(USR)] = D;
    }

    for (auto &It : ByUSR) {
      const clang::NamedDecl *D = It.second;
      std::string Old = D->getNameAsString();
      std::string New = toUpsideDown(Old);

      if (Verbose) {
        llvm::errs() << "Processing: " << Old << " -> " << New << "\n";
      }

      ReferenceFinder Finder(D, SM, Repls, New, Old);
      Finder.TraverseDecl(Ctx.getTranslationUnitDecl());
    }

    StringLiteralTransformer SLT(SM, Repls);
    SLT.TraverseDecl(Ctx.getTranslationUnitDecl());

    ExternalSymbolTracker ExtTracker(SM);
    ExtTracker.TraverseDecl(Ctx.getTranslationUnitDecl());

    std::map<const clang::FunctionDecl*, std::map<std::string, std::string>> FunctionAliasMap;

    for (auto &Entry : ExtTracker.FunctionExternals) {
      const clang::FunctionDecl *FD = Entry.first;
      const auto &Externals = Entry.second;

      if (Externals.empty()) continue;

      const clang::CompoundStmt *Body = llvm::dyn_cast_or_null<clang::CompoundStmt>(FD->getBody());
      if (!Body) continue;

      clang::SourceLocation BodyStart = Body->getLBracLoc().getLocWithOffset(1);

      std::string AliasDecls = "\n";
      std::map<std::string, std::string> &AliasMap = FunctionAliasMap[FD];

      for (const auto &ExtName : Externals) {
        size_t LastColon = ExtName.rfind("::");
        std::string UnqualifiedName = (LastColon != std::string::npos)
            ? ExtName.substr(LastColon + 2)
            : ExtName;

        std::string AliasName = toUpsideDown(UnqualifiedName);
        AliasMap[ExtName] = AliasName;

        AliasDecls += "    auto &" + AliasName + " = " + ExtName + ";\n";
      }

      auto Err = Repls.add(clang::tooling::Replacement(SM, BodyStart, 0, AliasDecls));
      if (Err) {
        llvm::consumeError(std::move(Err));
      }
    }

    ExternalSymbolReplacer ExtReplacer(SM, Repls, FunctionAliasMap);
    ExtReplacer.TraverseDecl(Ctx.getTranslationUnitDecl());

    if (Verbose) {
      llvm::errs() << "Total replacements: " << Repls.size() << "\n";
    }

    clang::Rewriter RW(SM, Ctx.getLangOpts());

    if (!clang::tooling::applyAllReplacements(Repls, RW)) {
      llvm::errs() << "Failed to apply replacements\n";
      return;
    }

    const clang::RewriteBuffer *RB = RW.getRewriteBufferFor(SM.getMainFileID());
    if (RB) {
      RB->write(llvm::outs());
    } else {
      llvm::outs() << SM.getBufferData(SM.getMainFileID());
    }
  }

private:
  std::unique_ptr<clang::ast_matchers::MatchFinder> Finder;
  std::unique_ptr<CollectDecls> Callback;
  std::vector<const clang::NamedDecl*> Decls;
  clang::tooling::Replacements Repls;
};

// xprefix_rename.cpp
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "clang/Tooling/Refactoring.h"
#include "clang/Tooling/Core/Replacement.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "llvm/Support/CommandLine.h"

using namespace clang;
using namespace clang::ast_matchers;
using namespace clang::tooling;

static llvm::cl::OptionCategory Cat("xprefix-renamer");

static bool isUserLocation(const SourceManager &SM, SourceLocation Loc) {
  if (Loc.isInvalid()) return false;
  Loc = SM.getExpansionLoc(Loc);
  if (SM.isInSystemHeader(Loc)) return false;
  if (SM.isInExternCSystemHeader(Loc)) return false;
  return SM.isWrittenInMainFile(Loc);
}

static bool shouldRename(const NamedDecl *D, ASTContext &Ctx) {
  if (!D) return false;
  if (D->isImplicit()) return false;

  const auto &SM = Ctx.getSourceManager();
  if (!isUserLocation(SM, D->getLocation())) return false;

  // Skip anonymous things
  if (!D->getIdentifier()) return false;

  // Skip main
  if (const auto *FD = dyn_cast<FunctionDecl>(D)) {
    if (FD->isMain()) return false;
  }

  // Skip anything declared inside system headers (already covered),
  // but also skip if its canonical decl isn't in main file.
  const NamedDecl *Canon = dyn_cast<NamedDecl>(D->getCanonicalDecl());
  if (Canon && !isUserLocation(SM, Canon->getLocation())) return false;

  return true;
}

namespace {
class CollectDecls : public MatchFinder::MatchCallback {
public:
  explicit CollectDecls(std::vector<const NamedDecl*> &Out) : Out(Out) {}

  void run(const MatchFinder::MatchResult &Result) override {
    auto *D = Result.Nodes.getNodeAs<NamedDecl>("nd");
    if (!D) return;
    if (!shouldRename(D, *Result.Context)) return;

    // Only rename the canonical to avoid duplicates
    const NamedDecl *Canon = dyn_cast<NamedDecl>(D->getCanonicalDecl());
    if (!Canon) Canon = D;

    Out.push_back(Canon);
  }

private:
  std::vector<const NamedDecl*> &Out;
};

class XPrefixAction : public ASTFrontendAction {
public:
  bool BeginSourceFileAction(CompilerInstance &CI) override {
    Repls.clear();
    return true;
  }

  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 StringRef InFile) override {
    Decls.clear();
    Finder = std::make_unique<MatchFinder>();
    Callback = std::make_unique<CollectDecls>(Decls);

    // Match most user-defined named decls (you can refine this set).
    Finder->addMatcher(namedDecl().bind("nd"), Callback.get());

    return Finder->newASTConsumer();
  }

  void EndSourceFileAction() override {
    auto &CI = getCompilerInstance();
    ASTContext &Ctx = CI.getASTContext();

    // Deduplicate by USR (stable symbol id).
    llvm::DenseMap<std::string, const NamedDecl*> ByUSR;

    for (const NamedDecl *D : Decls) {
      std::string USR;
      if (index::generateUSRForDecl(D, USR)) continue;
      ByUSR[USR] = D;
    }

    // Build replacements via rename refactoring.
    // NOTE: API may differ by LLVM version; adjust as needed.
    for (auto &It : ByUSR) {
      const NamedDecl *D = It.second;
      std::string Old = D->getNameAsString();
      std::string New = "x_" + Old;

      // Avoid double-prefixing if already renamed
      if (Old.rfind("x_", 0) == 0) continue;

      // Compute replacements for this symbol
      // This is the part that can vary most by LLVM version.
      auto Rename = createRenameOccurrences(
          Ctx, *D, New, /*PrevName=*/Old);

      if (!Rename) continue;

      for (const auto &R : *Rename) {
        llvm::Error Err = Repls.add(R);
        (void)Err; // ignore/collect errors as desired
      }
    }

    // Apply replacements to files.
    // In real tools you’ll want to write to disk or to stdout.
    LangOptions DefaultLang;
    IntrusiveRefCntPtr<DiagnosticOptions> DiagOpts(new DiagnosticOptions());
    DiagnosticsEngine Diags(
        IntrusiveRefCntPtr<DiagnosticIDs>(new DiagnosticIDs()),
        &*DiagOpts);

    auto &SM = Ctx.getSourceManager();
    Rewriter RW(SM, Ctx.getLangOpts());

    bool Failed = applyAllReplacements(Repls, RW);
    if (Failed) return;

    // Write modified buffers back to stdout
    RW.getEditBuffer(SM.getMainFileID()).write(llvm::outs());
  }

private:
  std::unique_ptr<MatchFinder> Finder;
  std::unique_ptr<CollectDecls> Callback;
  std::vector<const NamedDecl*> Decls;
  Replacements Repls;
};
} // namespace

int main(int argc, const char **argv) {
  auto ExpectedParser = CommonOptionsParser::create(argc, argv, Cat);
  if (!ExpectedParser) {
    llvm::errs() << ExpectedParser.takeError();
    return 1;
  }
  CommonOptionsParser &OptionsParser = ExpectedParser.get();
  ClangTool Tool(OptionsParser.getCompilations(),
                 OptionsParser.getSourcePathList());

  return Tool.run(newFrontendActionFactory<XPrefixAction>().get());
}

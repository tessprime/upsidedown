// xprefix_rename.cpp
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "clang/Tooling/Refactoring.h"
#include "clang/Tooling/Core/Replacement.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/Index/USRGeneration.h"
#include "llvm/Support/CommandLine.h"
#include <algorithm>
#include <map>
#include <vector>

using namespace clang;
using namespace clang::ast_matchers;
using namespace clang::tooling;

static llvm::cl::OptionCategory Cat("xprefix-renamer");

// Map of ASCII characters to upside-down Unicode equivalents
static const std::map<char, std::string> upsideDownMap = {
  // Lowercase letters
  {'a', "ɐ"}, {'b', "q"}, {'c', "ɔ"}, {'d', "p"}, {'e', "ǝ"},
  {'f', "ɟ"}, {'g', "ƃ"}, {'h', "ɥ"}, {'i', "ᴉ"}, {'j', "ɾ"},
  {'k', "ʞ"}, {'l', "l"}, {'m', "ɯ"}, {'n', "u"}, {'o', "o"},
  {'p', "d"}, {'q', "b"}, {'r', "ɹ"}, {'s', "s"}, {'t', "ʇ"},
  {'u', "n"}, {'v', "ʌ"}, {'w', "ʍ"}, {'x', "x"}, {'y', "ʎ"},
  {'z', "z"},

  // Uppercase letters
  {'A', "∀"}, {'B', "ᙠ"}, {'C', "Ɔ"}, {'D', "ᗡ"}, {'E', "Ǝ"},
  {'F', "Ⅎ"}, {'G', "⅁"}, {'H', "H"}, {'I', "I"}, {'J', "ſ"},
  {'K', "⋊"}, {'L', "˥"}, {'M', "W"}, {'N', "N"}, {'O', "O"},
  {'P', "Ԁ"}, {'Q', "Ό"}, {'R', "ᴚ"}, {'S', "S"}, {'T', "⊥"},
  {'U', "∩"}, {'V', "Λ"}, {'W', "M"}, {'X', "X"}, {'Y', "⅄"},
  {'Z', "Z"},

  // Numbers
  {'0', "0"}, {'1', "Ɩ"}, {'2', "ᄅ"}, {'3', "Ɛ"}, {'4', "ㄣ"},
  {'5', "ϛ"}, {'6', "9"}, {'7', "ㄥ"}, {'8', "8"}, {'9', "6"},

  // Common punctuation
  {'_', "‾"}, {'.', "˙"}, {',', "'"}, {'!', "¡"}, {'?', "¿"},
  {'\'', ","}, {'"', "„"}, {';', "؛"}, {'(', ")"}, {')', "("},
  {'[', "]"}, {']', "["}, {'{', "}"}, {'}', "{"}, {'<', ">"},
  {'>', "<"}, {'&', "⅋"}
};

// Convert a string to upside-down using the map
static std::string toUpsideDown(const std::string &str) {
  std::vector<std::string> chars;
  // Convert each character and store in vector
  for (char c : str) {
    auto it = upsideDownMap.find(c);
    if (it != upsideDownMap.end()) {
      chars.push_back(it->second);
    } else {
      // Keep character as-is if no mapping exists
      chars.push_back(std::string(1, c));
    }
  }
  // Reverse the vector of characters (not bytes!)
  std::reverse(chars.begin(), chars.end());

  // Concatenate into final string
  std::string result;
  for (const auto &ch : chars) {
    result += ch;
  }
  return result;
}

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

// Helper to find all references to a declaration
class ReferenceFinder : public RecursiveASTVisitor<ReferenceFinder> {
public:
  ReferenceFinder(const NamedDecl *Target, const SourceManager &SM,
                  Replacements &Repls, const std::string &NewName,
                  const std::string &OldName)
      : Target(Target->getCanonicalDecl()), SM(SM), Repls(Repls),
        NewName(NewName), OldNameLength(OldName.length()) {}

  bool VisitDeclRefExpr(DeclRefExpr *E) {
    const ValueDecl *D = E->getDecl();
    if (!D) return true;
    if (D->getCanonicalDecl() == Target) {
      addReplacement(E->getLocation(), OldNameLength);
    }
    return true;
  }

  bool VisitMemberExpr(MemberExpr *E) {
    const ValueDecl *D = E->getMemberDecl();
    if (!D) return true;
    if (D->getCanonicalDecl() == Target) {
      addReplacement(E->getMemberLoc(), OldNameLength);
    }
    return true;
  }

  bool VisitTypeLoc(TypeLoc TL) {
    if (auto ElabTL = TL.getAs<ElaboratedTypeLoc>()) {
      TL = ElabTL.getNamedTypeLoc();
    }

    if (auto TagTL = TL.getAs<TagTypeLoc>()) {
      const TagDecl *TD = TagTL.getDecl();
      if (TD && TD->getCanonicalDecl() == Target) {
        addReplacement(TagTL.getNameLoc(), OldNameLength);
      }
    } else if (auto TDT = TL.getAs<TypedefTypeLoc>()) {
      const TypedefNameDecl *TD = TDT.getTypedefNameDecl();
      if (TD && TD->getCanonicalDecl() == Target) {
        addReplacement(TDT.getNameLoc(), OldNameLength);
      }
    }
    return true;
  }

  bool VisitNamedDecl(NamedDecl *D) {
    if (D->getCanonicalDecl() == Target) {
      addReplacement(D->getLocation(), OldNameLength);
    }
    return true;
  }

private:
  void addReplacement(SourceLocation Loc, unsigned Length) {
    if (Loc.isInvalid()) return;
    Loc = SM.getExpansionLoc(Loc);
    if (Loc.isInvalid()) return;

    // Only replace in main file
    if (!SM.isWrittenInMainFile(Loc)) return;

    auto Err = Repls.add(Replacement(SM, Loc, Length, NewName));
    if (Err) {
      // Silently consume duplicate errors
      llvm::consumeError(std::move(Err));
    }
  }

  const Decl *Target;
  const SourceManager &SM;
  Replacements &Repls;
  std::string NewName;
  unsigned OldNameLength;
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
    auto &SM = Ctx.getSourceManager();

    // Deduplicate by USR (stable symbol id).
    std::map<std::string, const NamedDecl*> ByUSR;

    for (const NamedDecl *D : Decls) {
      llvm::SmallString<128> USR;
      if (index::generateUSRForDecl(D, USR)) continue;
      ByUSR[std::string(USR)] = D;
    }

    // Build replacements by finding all references
    for (auto &It : ByUSR) {
      const NamedDecl *D = It.second;
      std::string Old = D->getNameAsString();
      std::string New = toUpsideDown(Old);

      llvm::errs() << "Processing: " << Old << " -> " << New << "\n";

      // Find all references to this declaration and create replacements
      ReferenceFinder Finder(D, SM, Repls, New, Old);
      Finder.TraverseDecl(Ctx.getTranslationUnitDecl());
    }

    llvm::errs() << "Total replacements: " << Repls.size() << "\n";

    // Apply replacements to files.
    Rewriter RW(SM, Ctx.getLangOpts());

    // applyAllReplacements returns true on success in LLVM-16
    if (!tooling::applyAllReplacements(Repls, RW)) {
      llvm::errs() << "Failed to apply replacements\n";
      return;
    }

    // Write modified buffers back to stdout
    const RewriteBuffer *RB = RW.getRewriteBufferFor(SM.getMainFileID());
    if (RB) {
      RB->write(llvm::outs());
    } else {
      // No changes, write original
      llvm::outs() << SM.getBufferData(SM.getMainFileID());
    }
  }

private:
  std::unique_ptr<MatchFinder> Finder;
  std::unique_ptr<CollectDecls> Callback;
  std::vector<const NamedDecl*> Decls;
  Replacements Repls;
};
} // namespace

int main(int argc, const char **argv) {
  // Reset command line parser to avoid "registered more than once" errors
  llvm::cl::ResetCommandLineParser();

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

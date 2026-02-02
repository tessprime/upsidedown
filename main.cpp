// main.cpp
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
#include "clang/Lex/Lexer.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <cstdio>
#include <map>
#include <set>
#include <vector>

using namespace clang;
using namespace clang::ast_matchers;
using namespace clang::tooling;

static llvm::cl::OptionCategory Cat("xprefix-renamer");

// Verbose flag (initialized in main after ResetCommandLineParser)
static bool Verbose = false;

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

// Escape special characters for C++ string literals
static std::string escapeString(const std::string &str) {
  std::string result;
  for (unsigned char c : str) {
    switch (c) {
      case '\n': result += "\\n"; break;
      case '\t': result += "\\t"; break;
      case '\r': result += "\\r"; break;
      case '\0': result += "\\0"; break;
      case '\\': result += "\\\\"; break;
      case '"': result += "\\\""; break;
      case '\a': result += "\\a"; break;
      case '\b': result += "\\b"; break;
      case '\f': result += "\\f"; break;
      case '\v': result += "\\v"; break;
      default:
        // For printable ASCII and UTF-8 continuation bytes, keep as-is
        if (c >= 32 && c < 127) {
          result += c;
        } else if (c >= 0x80) {
          // UTF-8 multi-byte sequence, keep as-is
          result += c;
        } else {
          // Other control characters, use octal escape
          char buf[5];
          snprintf(buf, sizeof(buf), "\\%03o", c);
          result += buf;
        }
        break;
    }
  }
  return result;
}

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

// Visitor to transform string literals
class StringLiteralTransformer : public RecursiveASTVisitor<StringLiteralTransformer> {
public:
  StringLiteralTransformer(const SourceManager &SM, Replacements &Repls)
      : SM(SM), Repls(Repls) {}

  bool VisitStringLiteral(StringLiteral *SL) {
    if (!SL) return true;

    SourceLocation Loc = SL->getBeginLoc();
    if (Loc.isInvalid()) return true;
    Loc = SM.getExpansionLoc(Loc);

    // Only transform string literals in main file
    if (!SM.isWrittenInMainFile(Loc)) return true;

    // Get the interpreted string (with escape sequences already processed)
    std::string Interpreted = SL->getString().str();

    // Transform it
    std::string Transformed = toUpsideDown(Interpreted);

    // Escape it for writing back to source
    std::string Escaped = escapeString(Transformed);

    // Get the actual source text to determine the length
    // We need to get from opening quote to closing quote
    SourceRange Range = SL->getSourceRange();
    SourceLocation EndLoc = Range.getEnd();
    if (EndLoc.isInvalid()) return true;
    EndLoc = SM.getExpansionLoc(EndLoc);

    // Use Lexer to get the location after the closing quote
    SourceLocation AfterQuote = Lexer::getLocForEndOfToken(EndLoc, 0, SM, LangOptions());

    // Calculate length of content between quotes in source
    // Loc points to opening quote, AfterQuote points after closing quote
    // So we subtract 2 (one for each quote)
    unsigned TotalLength = SM.getFileOffset(AfterQuote) - SM.getFileOffset(Loc);
    unsigned ContentLength = TotalLength - 2; // Exclude both quotes

    // Replace the content between the quotes
    SourceLocation StartLoc = Loc.getLocWithOffset(1);

    auto Err = Repls.add(Replacement(SM, StartLoc, ContentLength, Escaped));
    if (Err) {
      llvm::consumeError(std::move(Err));
    }

    return true;
  }

private:
  const SourceManager &SM;
  Replacements &Repls;
};

// Track external symbol usage per function
class ExternalSymbolTracker : public RecursiveASTVisitor<ExternalSymbolTracker> {
public:
  ExternalSymbolTracker(const SourceManager &SM)
      : SM(SM), CurrentFunction(nullptr) {}

  bool VisitFunctionDecl(FunctionDecl *FD) {
    if (!FD || !FD->hasBody()) return true;
    if (!isUserLocation(SM, FD->getLocation())) return true;

    CurrentFunction = FD;
    return true;
  }

  bool VisitDeclRefExpr(DeclRefExpr *E) {
    if (!E || !CurrentFunction) return true;

    const ValueDecl *D = E->getDecl();
    if (!D) return true;

    // Skip operators and special functions
    if (const auto *FD = dyn_cast<FunctionDecl>(D)) {
      if (FD->isOverloadedOperator()) return true;
    }

    // Check if this is an external symbol (not defined in main file)
    SourceLocation DeclLoc = D->getLocation();
    if (DeclLoc.isInvalid()) return true;
    DeclLoc = SM.getExpansionLoc(DeclLoc);

    // If the declaration is not in the main file, it's external
    if (!SM.isWrittenInMainFile(DeclLoc)) {
      // Get the full qualified name
      std::string FullName;
      llvm::raw_string_ostream OS(FullName);
      E->getDecl()->printQualifiedName(OS);
      OS.flush();

      FunctionExternals[CurrentFunction].insert(FullName);
      ExternalDeclMap[FullName] = E->getDecl();
    }

    return true;
  }

  bool VisitMemberExpr(MemberExpr *E) {
    if (!E || !CurrentFunction) return true;

    const ValueDecl *D = E->getMemberDecl();
    if (!D) return true;

    SourceLocation DeclLoc = D->getLocation();
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

  // Map of function -> set of external symbols it uses
  std::map<const FunctionDecl*, std::set<std::string>> FunctionExternals;
  // Map of external symbol name -> its declaration
  std::map<std::string, const NamedDecl*> ExternalDeclMap;

private:
  const SourceManager &SM;
  FunctionDecl *CurrentFunction;
};

// Replace external symbol uses with aliases
class ExternalSymbolReplacer : public RecursiveASTVisitor<ExternalSymbolReplacer> {
public:
  ExternalSymbolReplacer(const SourceManager &SM, Replacements &Repls,
                         const std::map<const FunctionDecl*, std::map<std::string, std::string>> &AliasMap)
      : SM(SM), Repls(Repls), AliasMap(AliasMap), CurrentFunction(nullptr) {}

  bool VisitFunctionDecl(FunctionDecl *FD) {
    if (!FD || !FD->hasBody()) return true;
    CurrentFunction = FD;
    return true;
  }

  bool VisitDeclRefExpr(DeclRefExpr *E) {
    if (!E || !CurrentFunction) return true;

    auto FuncIt = AliasMap.find(CurrentFunction);
    if (FuncIt == AliasMap.end()) return true;

    const ValueDecl *D = E->getDecl();
    if (!D) return true;

    std::string FullName;
    llvm::raw_string_ostream OS(FullName);
    D->printQualifiedName(OS);
    OS.flush();

    auto AliasIt = FuncIt->second.find(FullName);
    if (AliasIt != FuncIt->second.end()) {
      // Replace this use with the alias
      SourceLocation Loc = E->getLocation();
      if (Loc.isInvalid()) return true;
      Loc = SM.getExpansionLoc(Loc);

      if (!SM.isWrittenInMainFile(Loc)) return true;

      // Calculate the actual source range length
      SourceRange Range = E->getSourceRange();
      if (Range.isInvalid()) return true;

      // Get the actual text length from the source
      auto StartLoc = Range.getBegin();
      auto EndLoc = Range.getEnd();

      // Find the end of the identifier token
      EndLoc = clang::Lexer::getLocForEndOfToken(EndLoc, 0, SM, clang::LangOptions());

      unsigned Length = SM.getFileOffset(EndLoc) - SM.getFileOffset(StartLoc);

      auto Err = Repls.add(Replacement(SM, Range.getBegin(), Length, AliasIt->second));
      if (Err) {
        llvm::consumeError(std::move(Err));
      }
    }

    return true;
  }

private:
  const SourceManager &SM;
  Replacements &Repls;
  const std::map<const FunctionDecl*, std::map<std::string, std::string>> &AliasMap;
  FunctionDecl *CurrentFunction;
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

      if (Verbose) {
        llvm::errs() << "Processing: " << Old << " -> " << New << "\n";
      }

      // Find all references to this declaration and create replacements
      ReferenceFinder Finder(D, SM, Repls, New, Old);
      Finder.TraverseDecl(Ctx.getTranslationUnitDecl());
    }

    // Transform string literals
    StringLiteralTransformer SLT(SM, Repls);
    SLT.TraverseDecl(Ctx.getTranslationUnitDecl());

    // Track external symbols and create aliases
    ExternalSymbolTracker ExtTracker(SM);
    ExtTracker.TraverseDecl(Ctx.getTranslationUnitDecl());

    // Build alias map for all functions
    std::map<const FunctionDecl*, std::map<std::string, std::string>> FunctionAliasMap;

    // For each function, insert alias declarations
    for (auto &Entry : ExtTracker.FunctionExternals) {
      const FunctionDecl *FD = Entry.first;
      const auto &Externals = Entry.second;

      if (Externals.empty()) continue;

      // Get the start of the function body
      const CompoundStmt *Body = dyn_cast_or_null<CompoundStmt>(FD->getBody());
      if (!Body) continue;

      SourceLocation BodyStart = Body->getLBracLoc().getLocWithOffset(1);

      // Build the alias declarations
      std::string AliasDecls = "\n";
      std::map<std::string, std::string> &AliasMap = FunctionAliasMap[FD];

      for (const auto &ExtName : Externals) {
        // Extract the unqualified name for aliasing
        size_t LastColon = ExtName.rfind("::");
        std::string UnqualifiedName = (LastColon != std::string::npos)
            ? ExtName.substr(LastColon + 2)
            : ExtName;

        std::string AliasName = toUpsideDown(UnqualifiedName);
        AliasMap[ExtName] = AliasName;

        AliasDecls += "    auto &" + AliasName + " = " + ExtName + ";\n";
      }

      // Insert the alias declarations at the start of the function
      auto Err = Repls.add(Replacement(SM, BodyStart, 0, AliasDecls));
      if (Err) {
        llvm::consumeError(std::move(Err));
      }
    }

    // Replace uses of external symbols with aliases
    ExternalSymbolReplacer ExtReplacer(SM, Repls, FunctionAliasMap);
    ExtReplacer.TraverseDecl(Ctx.getTranslationUnitDecl());

    if (Verbose) {
      llvm::errs() << "Total replacements: " << Repls.size() << "\n";
    }

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

  // Define verbose flag after reset
  llvm::cl::opt<bool> VerboseOpt("v", llvm::cl::desc("Enable verbose output"),
                                  llvm::cl::cat(Cat));

  auto ExpectedParser = CommonOptionsParser::create(argc, argv, Cat);
  if (!ExpectedParser) {
    llvm::errs() << ExpectedParser.takeError();
    return 1;
  }
  CommonOptionsParser &OptionsParser = ExpectedParser.get();

  // Set the global verbose flag
  Verbose = VerboseOpt;

  ClangTool Tool(OptionsParser.getCompilations(),
                 OptionsParser.getSourcePathList());

  return Tool.run(newFrontendActionFactory<XPrefixAction>().get());
}

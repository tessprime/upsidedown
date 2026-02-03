// main.cpp
#include "llvm/Support/CommandLine.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/raw_ostream.h"
#include "upside_down_action.h"

using namespace clang;
using namespace clang::tooling;

#include "upsidedown_utils.h"

static llvm::cl::OptionCategory Cat("upsidedown options");

// Define the global Debug flag (declared in upsidedown_utils.h)
bool Debug = false;

int main(int argc, const char **argv) {
  // Reset command line parser to avoid "registered more than once" errors
  llvm::cl::ResetCommandLineParser();

  // Define debug flag after reset
  llvm::cl::opt<bool> DebugOpt("d", llvm::cl::desc("Enable debug output"),
                                  llvm::cl::cat(Cat));

  auto ExpectedParser = CommonOptionsParser::create(argc, argv, Cat);
  if (!ExpectedParser) {
    llvm::errs() << ExpectedParser.takeError();
    return 1;
  }
  CommonOptionsParser &OptionsParser = ExpectedParser.get();

  // Set the global debug flag
  Debug = DebugOpt;

  ClangTool Tool(OptionsParser.getCompilations(),
                 OptionsParser.getSourcePathList());

  return Tool.run(newFrontendActionFactory<UpsideDownAction>().get());
}

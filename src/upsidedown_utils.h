// upsidedown_utils.h
#pragma once

#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/Lex/Lexer.h"
#include <algorithm>
#include <cstdio>
#include <map>
#include <string>
#include <vector>

// Global verbose flag (defined in one translation unit)
extern bool Verbose;

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
  {'\'', ","}, {'\"', "„"}, {';', "؛"}, {'(', ")"}, {')', "("},
  {'[', "]"}, {']', "["}, {'{', "}"}, {'}', "{"}, {'<', ">"},
  {'>', "<"}, {'&', "⅋"}
};

// Escape special characters for C++ string literals
static inline std::string escapeString(const std::string &str) {
  std::string result;
  for (unsigned char c : str) {
    switch (c) {
      case '\n': result += "\\n"; break;
      case '\t': result += "\\t"; break;
      case '\r': result += "\\r"; break;
      case '\0': result += "\\0"; break;
      case '\\': result += "\\\\"; break;
      case '\"': result += "\\\""; break;
      case '\a': result += "\\a"; break;
      case '\b': result += "\\b"; break;
      case '\f': result += "\\f"; break;
      case '\v': result += "\\v"; break;
      default:
        if (c >= 32 && c < 127) {
          result += c;
        } else if (c >= 0x80) {
          result += c;
        } else {
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
static inline std::string toUpsideDown(const std::string &str, bool ignoreUnderscore = true) {
  std::vector<std::string> chars;
  for (char c : str) {
    auto it = upsideDownMap.find(c);
    if (it != upsideDownMap.end() && !(ignoreUnderscore && c == '_')) {
      chars.push_back(it->second);
    } else {
      chars.push_back(std::string(1, c));
    }
  }
  std::reverse(chars.begin(), chars.end());

  std::string result;
  for (const auto &ch : chars) result += ch;
  return result;
}

static inline bool isUserLocation(const clang::SourceManager &SM, clang::SourceLocation Loc) {
  if (Loc.isInvalid()) return false;
  Loc = SM.getExpansionLoc(Loc);
  if (SM.isInSystemHeader(Loc)) return false;
  if (SM.isInExternCSystemHeader(Loc)) return false;
  return SM.isWrittenInMainFile(Loc);
}

static inline bool shouldRename(const clang::NamedDecl *D, clang::ASTContext &Ctx) {
  if (!D) return false;
  if (D->isImplicit()) return false;

  const auto &SM = Ctx.getSourceManager();
  if (!isUserLocation(SM, D->getLocation())) return false;

  if (!D->getIdentifier()) return false;

  if (const auto *FD = llvm::dyn_cast<clang::FunctionDecl>(D)) {
    if (FD->isMain()) return false;
  }

  const clang::NamedDecl *Canon = llvm::dyn_cast<clang::NamedDecl>(D->getCanonicalDecl());
  if (Canon && !isUserLocation(SM, Canon->getLocation())) return false;

  return true;
}

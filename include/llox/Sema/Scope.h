#ifndef LLOX_SEMA_SCOPE_H
#define LLOX_SEMA_SCOPE_H

#include "llox/AST/AST.h"
#include "llox/Basic/LLVM.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"

namespace llox {

class Scope {
   Scope *Parent;
   llvm::StringMap<Decl*> Symbols;

public:
   Scope(Scope *Parent = nullptr)
      : Parent(Parent) {};

   Scope *getParent();
   bool insert(Decl *Declaration);
   Decl *lookup(StringRef Name);
};


} // namespace llox

#endif
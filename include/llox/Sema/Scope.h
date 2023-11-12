#ifndef LLOX_SEMA_SCOPE_H
#define LLOX_SEMA_SCOPE_H

namespace llox {

class Scope {
   Scope *Parent;
   llvm::StringMap<Decl*> Symbols;

public:
   Scope(Scope *Parent = nullptr);

   Scope *getParent();
   bool insert(Decl *Declaration);
   Decl *lookup(StringRef Name);
};


} // namespace llox

#endif
#include "llox/Sema/Scope.h"

namespace llox {

Scope::Scope(Scope *Parent = nullptr)
   : Parent(Parent) {}

Scope *Scope::getParent() {
   return Parent;
}

bool Scope::insert(Decl *Declaration) {
   return Symbols.insert(std::make_pair(Declaration->getName(), Declaration)).second;
}

Decl *Scope::lookup(StringRef Name) {
   Scope *S = this;
   while (S) {
      const auto I = S->Symbols.find(Name);
      if (I != S->Symbols.end())
         return I->second;
      S = S->getParent();
   }
   return nullptr;
}

} // namespace llox

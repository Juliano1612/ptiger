#include "ptiger-scope.h"

namespace Ptiger
{

Scope::Scope ()
{
}

void
Scope::push_scope ()
{
  map_stack.push_back (SymbolMapping());
}

void
Scope::push_scope_fn() {
    func_map_stack.push_back(FuncMapping());
}

void
Scope::pop_scope ()
{
  gcc_assert (!map_stack.empty());
  map_stack.pop_back ();
}

void
   Scope::pop_scope_fn() {
       gcc_assert(!func_map_stack.empty());
       func_map_stack.pop_back();
}

SymbolPtr
Scope::lookup (const std::string &str)
{
  for (MapStack::reverse_iterator map = map_stack.rbegin ();
       map != map_stack.rend (); map++)
    {
      if (SymbolPtr sym = map->get (str))
	{
	  return sym;
	}
    }
  return SymbolPtr();
}

FuncPtr
   Scope::lookup_fn(const std::string &str) {
       for (FuncMapStack::reverse_iterator funcmap = func_map_stack.rbegin();
            funcmap != func_map_stack.rend(); funcmap++) {
           if (FuncPtr fun = funcmap->get(str)) {
               return fun;
           }
       }
       return FuncPtr();
}
}

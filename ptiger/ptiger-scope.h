#ifndef PTIGER_SCOPE_H
#define PTIGER_SCOPE_H

#include "ptiger-symbol-mapping.h"
#include "ptiger-func-mapping.h"
#include <tr1/memory>
#include <vector>

namespace Ptiger
{

struct Scope
{
public:
  SymbolMapping &
  get_current_mapping ()
  {
    gcc_assert (!map_stack.empty ());
    return map_stack.back ();
  }

  FuncMapping &
        get_current_mapping_fn() {
            gcc_assert(!func_map_stack.empty());
            return func_map_stack.back();
  }

  void push_scope ();
  void push_scope_fn();
  void pop_scope ();
  void pop_scope_fn();

  Scope ();

  SymbolPtr lookup (const std::string &str);
  FuncPtr lookup_fn(const std::string &str);

private:
  typedef std::vector<SymbolMapping> MapStack;
  MapStack map_stack;
  typedef std::vector <FuncMapping> FuncMapStack;
  FuncMapStack func_map_stack;
};

}

#endif // PTIGER_SCOPE_H

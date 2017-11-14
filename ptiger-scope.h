#ifndef PTIGER_SCOPE_H
#define PTIGER_SCOPE_H

#include "ptiger-symbol-mapping.h"
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

  void push_scope ();
  void pop_scope ();

  Scope ();

  SymbolPtr lookup (const std::string &str);

private:
  typedef std::vector<SymbolMapping> MapStack;
  MapStack map_stack;
};

}

#endif // PTIGER_SCOPE_H

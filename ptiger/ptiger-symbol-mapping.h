#ifndef PTIGER_SYMBOL_MAPPING_H
#define PTIGER_SYMBOL_MAPPING_H

#include "ptiger/ptiger-symbol.h"
#include <tr1/memory>
#include <map>

namespace Ptiger
{

struct SymbolMapping
{
public:

  void insert (SymbolPtr s);
  SymbolPtr get (const std::string &str) const;

private:

  typedef std::map<std::string, SymbolPtr > Map;
  Map map;
};

}

#endif // PTIGER_SYMBOL_MAPPING_H

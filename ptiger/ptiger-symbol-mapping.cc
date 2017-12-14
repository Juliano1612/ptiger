#include <utility>
#include <sstream>

#include "ptiger-symbol-mapping.h"

#include "config.h"
#include "system.h"

namespace Ptiger {

void
SymbolMapping::insert(SymbolPtr s) {
        if(s != NULL)
                if(s->get_kind() != Ptiger::FORVARIABLE)
                        gcc_assert(true);
        // gcc_assert(s != NULL && s->get_kind() != Ptiger::FORVARIABLE);
        if(s != NULL && s->get_kind() == Ptiger::FORVARIABLE) map.erase(s->get_name().c_str());
        std::pair<Map::iterator, bool> p
                = map.insert(std::make_pair(s->get_name(), s));

        gcc_assert(p.second);
}

SymbolPtr
SymbolMapping::get (const std::string &str) const
{
        Map::const_iterator it = map.find (str);
        if (it != map.end ())
        {
                return it->second;
        }
        return SymbolPtr();
}

}

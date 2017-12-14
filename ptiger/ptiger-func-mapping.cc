#include <utility>
#include <sstream>

#include "ptiger-func-mapping.h"

#include "config.h"
#include "system.h"

namespace Ptiger {

    void
    FuncMapping::insert(FuncPtr s) {
        gcc_assert(s != NULL);
        std::pair<FuncMap::iterator, bool> p
                = funcmap.insert(std::make_pair(s->get_name(), s));

        gcc_assert(p.second);
    }

    FuncPtr
    FuncMapping::get(const std::string &str) const {
        FuncMap::const_iterator it = funcmap.find(str);
        if (it != funcmap.end()) {
            return it->second;
        }
        return FuncPtr();
    }

}

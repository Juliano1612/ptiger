#ifndef PTIGER_FUNC_MAPPING_H
#define PTIGER_FUNC_MAPPING_H

#include "ptiger/ptiger-func.h"
#include <tr1/memory>
#include <map>

namespace Ptiger {

    struct FuncMapping {
    public:

        void insert(FuncPtr s);

        FuncPtr get(const std::string &str) const;

    private:

        typedef std::map <std::string, FuncPtr> FuncMap;
        FuncMap funcmap;
    };

}

#endif // PTIGER_FUNC_MAPPING_H

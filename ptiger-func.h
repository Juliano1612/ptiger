#ifndef PTIGER_FUNC_H
#define PTIGER_FUNC_H

#include "ptiger/ptiger-tree.h"

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include <list>

#include <tr1/memory>

namespace Ptiger {

    enum /* class */ FuncKind {
        EXTERNAL,
        INTERNAL
    };

    struct Func {
    public:
        struct arg{
            tree arg_type;
            tree expr;
            std::string argname;
        };

        // Func(FuncKind kind, const std::string &name_, tree ret_type, std::list<struct arg> argslist, location_t locus) : kind(kind), name(name_), ret_type(ret_type), argslist(argslist), locus(locus), body(error_mark_node) {
        //     gcc_assert(name.size() > 0);
        // }

        Func(FuncKind kind, const std::string &name_, tree ret_type, std::list<struct arg> argslist) : kind(kind), name(name_), ret_type(ret_type), argslist(argslist), body(error_mark_node) {
            gcc_assert(name.size() > 0);
        }

       FuncKind
       get_kind() const {
           return kind;
       }

        std::string
        get_name() const {
            return name;
        }

        // void
        // set_tree_decl(Tree decl_) {
        //     gcc_assert((kind == VARIABLE && decl_.get_tree_code() == VAR_DECL)
        //                || (kind == TYPENAME && decl_.get_tree_code() == TYPE_DECL));
        //     decl = decl_;
        // }

        Tree
        get_func_body() const {
            return body;
        }

        tree
        get_ret_type() const {
            return ret_type;
        }

        int
        get_argslist_size() const {
            return argslist.size();
        }

        // location_t
        // get_locus() const {
        //     return locus;
        // }

        std::list<struct arg>
        get_argslist() const {
            return argslist;
        }

    private:
        FuncKind kind;
        std::string name;
        Tree body;
        std::list<struct arg> argslist;
        tree ret_type;
        // location_t locus;

    };

    typedef std::tr1::shared_ptr <Func> FuncPtr;
    typedef std::tr1::shared_ptr<const Func> const_FuncPtr;

}

#endif // PTIGER_FUNC_H

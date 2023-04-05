
#pragma once

#include "../structures/cfg.h"
#include "../structures/epresburger.h"

#include <map>
#include <string>

using namespace pres;
using namespace std;

class CFG2Pres {

    cfg::CFG const* cfg;
    map<string, pres_val_ptr> const* nonterm_vals;
    map<cfg::rule_const_ptr, pres_var_ptr> const* rules_vars;
    string const* var_prefix;


    public:
        CFG2Pres() {}

        // has free variables saying how many of each terminal to begin with
        // and how many times each rule is fired in a full derivation tree
        pres::pres_ptr translateCFG(cfg::CFG const& cfg,
                                    map<string, pres_val_ptr> const& nonterms_in,
                                    map<cfg::rule_const_ptr, pres_var_ptr> const& rules_out,
                                    string const& new_var_prefix);

        static void make_rule_vars(cfg::CFG const& cfg, 
                                   string const& prefix,
                                   map<cfg::rule_const_ptr, pres_var_ptr>& result);
        static void make_nonterm_vars(cfg::CFG const& cfg, 
                                      string const& prefix,
                                      map<string, pres_var_ptr>& result);
        static void make_term_vars(cfg::CFG const& cfg, 
                                   string const& prefix,
                                   map<string, pres_var_ptr>& result);

    private:

        pres_ptr make_counts_clause();
        pres_ptr make_nonterm_count_clause(string const& nonterm);
        pres_ptr make_counts_terms();
        pres_ptr make_connects_clause();

        pres_var_ptr get_rule_var(cfg::rule_const_ptr rule); 
        pres_val_ptr get_nonterm_val(string const& nonterm);

        pres_ptr make_not_generated(string const& character);
        void make_first_bullet(pres_assoc_bin_ptr clause,
                               map<string, pres_var_ptr> const& zs);
        void make_second_bullet(pres_assoc_bin_ptr clause,
                                map<string, pres_var_ptr> const& zs);

        void make_second_bullet_big_disjunct(string const& character,
                                             map<string, pres_var_ptr> const& zs,
                                             pres_val_ptr& zero,
                                             pres_val_ptr& one,
                                             pres_val_ptr& z,
                                             pres_assoc_bin_ptr& clause);

        template <class T, class U>
        U safe_lookup(T const& key, map<T, U> const* map);

        template <class T, class U> 
        static void make_map(T const& vars, 
                             string const& prefix,
                             map<U, pres_var_ptr>& result);
};

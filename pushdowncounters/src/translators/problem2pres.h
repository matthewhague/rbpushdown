
#pragma once

#include "../structures/pds.h"
#include "../structures/cfg.h"
#include "../structures/epresburger.h"
#include "../structures/problem.h"

#include <map>

class Problem2pres {

    static int const CHECK_ZERO_OFFSET = 0;
    static int const CHECK_UP_OFFSET = 1;
    static int const CHECK_DOWN_OFFSET = 2;
    static int const MODE_CYCLE = 3;

    pds::Pds const* pco;
    Problem const* prob;
    int nmodes_per_counter;
    int nmode_blocks;
    cfg::cfg_ptr* mode_cfgs;
    map<string, pres::pres_val_ptr>* counter_inc_counts;
    map<string, pres::pres_val_ptr>* counter_dec_counts;
    map<string, pres::pres_val_ptr>* nonterms_in;
    map<string, pres::pres_val_ptr> terms_out;
    map<cfg::rule_const_ptr, pres::pres_var_ptr> rule_vars;
    map<string, pres::pres_var_ptr>* mode_vars;
    pres::pres_val_ptr* mode_consts;    
    map<pres::pres_var_ptr, pres::pres_ptr> zero_checks;
    map<pres::pres_var_ptr, pres::pres_ptr> up_checks;
    map<pres::pres_var_ptr, pres::pres_ptr> down_checks;
    set<string> inits;


    public:
        Problem2pres() {}
        ~Problem2pres() {}

        pres::pres_ptr translateProblem(Problem const& problem);


    private:
        void add_rules_vars(cfg::cfg_const_ptr cfg, int i);
        pres::pres_ptr make_run_clauses();
        pres::pres_ptr make_run_clause(int i);
        pres::pres_var_ptr make_var(string const& ch, int i);
        void set_up_variables();
        void delete_variables();
        pres::pres_ptr make_valid_clauses();
        pres::pres_ptr make_valid_clause(int i);
        pres::pres_ptr make_connects_clauses();
        pres::pres_ptr make_connects_clause(int i);
        pres::pres_ptr make_zero_check(pres::pres_var_ptr& var);
        pres::pres_ptr make_up_check(pres::pres_var_ptr& var);
        pres::pres_ptr make_down_check(pres::pres_var_ptr& var);
        pres::pres_ptr make_check(pres::pres_var_ptr& var, 
                                  int offset,
                                  map<pres::pres_var_ptr, pres::pres_ptr>& memo);
        pres::pres_var_ptr get_rule_var(cfg::rule_const_ptr rule);
        pres::pres_ptr make_applicable(cfg::rule_const_ptr rule, int i);
        pres::pres_var_ptr get_mode_var(string const& counter, int i);
        pres::pres_ptr make_rule_guard(pds::rule_guard_ptr guard, int i);
        pres::pres_ptr make_rule_connect(cfg::rule_const_ptr rule, int i);
        pres::pres_val_ptr get_nonterm_val(string const& nonterm, int i);
        pres::pres_ptr make_sequence();
        pres::pres_ptr make_good_counter_ops();
        pres::pres_ptr make_start();
        void minimise_mode_cfgs();
        pres::pres_ptr quantify(pres::pres_ptr& fmla);
        void add_num_terms_produced(string const& nonterm, int i, pres::pres_plus_ptr& amount);
        void make_nonterms_in_map(int i, 
                                  set<string> const& nonterms, 
                                  map<string, pres::pres_val_ptr>& map);
        void make_terms_out_map(set<string> const& terms, 
                                map<string, pres::pres_val_ptr>& map);
        void make_counter_inc_counts(int i, 
                                     set<string> const& counters, 
                                     map<string, pres::pres_val_ptr>& map);
        void make_counter_dec_counts(int i, 
                                     set<string> const& counters, 
                                     map<string, pres::pres_val_ptr>& map);
        pres::pres_val_ptr get_counter_inc_counts(string const& counter, int i);
        pres::pres_val_ptr get_counter_dec_counts(string const& counter, int i);
        pres::pres_ptr make_user_constraint();
        pres::pres_val_ptr get_term_val(string const& term);

        template <class T, class U>
        U safe_lookup(T const& key, map<T, U>& map);

        template<class T>
        void quantify_map_vars(pres::pres_exists_ptr& efmla, map<T, pres::pres_var_ptr>& map);

        template <class V>
        void make_var_map(int i, set<string> const& chars, map<string, V>& map);

};

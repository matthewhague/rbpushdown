

#pragma once

#include "../structures/pds.h"
#include "../structures/cfg.h"

#include <set>

using namespace std;

class Pds2CFG {

    cfg::cfg_ptr cfg;
    pds::Pds const* pds;
    int start_mode;
    int end_mode;

    public:
        Pds2CFG() { }
        cfg::cfg_ptr translatePds(pds::Pds const& pds, int start_mode, int end_mode);

        static string counter_inc_term_name(string const& counter, int mode);
        static string counter_dec_term_name(string const& counter, int mode);
        static string term_name(string const& t);

        static void make_inits(string const& init_p,
                               string const& init_a,
                               string const& final_p,
                               int start_mode,
                               int end_mode,
                               set<string>& inits);

    private:
        void add_rules(pds::rule_const_ptr pds_rule);

        void add_rules_pop(pds::rule_const_ptr pds_rule);

        void add_rules_nonpop(pds::rule_const_ptr pds_rule);

        void add_rules_nonpop_dest(pds::rule_const_ptr pds_rule, 
                                   string const& dest_control);

        void make_seqs(int length, 
                       string const& start,
                       string const& finish,
                       set<string> const& items, 
                       set<vector<string>>& result);

        void make_mode_seqs(pds::rule_const_ptr rule, set<vector<int>>& result);

        void make_rhs(pds::rule_const_ptr rule,
                      vector<string> const& pop_seq,
                      vector<int> const& mode_seq,
                      vector<string>& result); 

        string make_grammar_char(string const& p, 
                                 string const& a, 
                                 string const& q,
                                 int mode_in,
                                 int mode_out);

        string make_term(string const& t);

        void add_terms(pds::rule_const_ptr rule, int mode, vector<string>& rhs);

        void add_cfg_rule(pds::rule_const_ptr rule,
                          vector<string> const& pop_seq,
                          vector<int> const& mode_seq,
                          string const& dest_control);

        void add_rule_to_cfg(string const& lhs,
                             vector<string> const& rhs,
                             pds::rule_const_ptr pds_rule,
                             bool change);

        static string counter_inc_term_name_no_prefix(string const& counter, int mode);
        static string counter_dec_term_name_no_prefix(string const& counter, int mode);
        static string name_grammar_char(string const& p, 
                                        string const& a, 
                                        string const& q,
                                        int mode_in,
                                        int mode_out);

};


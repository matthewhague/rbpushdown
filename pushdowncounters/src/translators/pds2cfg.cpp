
#include "pds2cfg.h"

#include "../structures/pds.h"
#include "../structures/cfg.h"

#include <set>
#include <iostream>
#include <assert.h>
#include <sstream>

#include <boost/make_shared.hpp>

using namespace std;


cfg::cfg_ptr Pds2CFG::translatePds(pds::Pds const& conv_pds, int first_mode, int last_mode) {
    cfg::cfg_ptr new_cfg(new cfg::CFG());
    cfg = new_cfg;
    pds = &conv_pds;
    start_mode = first_mode;
    end_mode = last_mode;

    auto pds_rules = pds->get_rules();
    for (auto it = pds_rules.begin(); it != pds_rules.end(); ++it) {
        add_rules(*it);
    }

    return cfg;
}



void Pds2CFG::add_rules(pds::rule_const_ptr pds_rule) {
    if (pds_rule->get_w().size() == 0) 
        add_rules_pop(pds_rule);
    else
        add_rules_nonpop(pds_rule);
}


void Pds2CFG::add_rules_pop(pds::rule_const_ptr pds_rule) {
    assert(pds_rule->get_w().size() == 0);

    auto p = pds_rule->get_p();
    auto a = pds_rule->get_a();
    auto q = pds_rule->get_q();

    // either we change mode or don't
    string n_paq1 = make_grammar_char(p, a, q, start_mode, start_mode);
    vector<string> rhs1;
    add_terms(pds_rule, start_mode, rhs1);
    add_rule_to_cfg(n_paq1, rhs1, pds_rule, false);

    if (start_mode < end_mode) {
        string n_paq2 = make_grammar_char(p, a, q, start_mode, start_mode + 1);
        vector<string> rhs2;
        add_terms(pds_rule, start_mode + 1, rhs2);
        add_rule_to_cfg(n_paq2, rhs2, pds_rule, true);
    }
}

void Pds2CFG::add_rules_nonpop(pds::rule_const_ptr pds_rule) {
    auto controls = pds->get_controls();
    for (auto it = controls.begin(); it != controls.end(); ++it) {
        add_rules_nonpop_dest(pds_rule, *it);
    }
}

void Pds2CFG::add_rules_nonpop_dest(pds::rule_const_ptr pds_rule, 
                                    string const& dest_control) {
    assert(pds_rule->get_w().size() > 0);

    auto q = pds_rule->get_q();
    auto w = pds_rule->get_w();
    auto controls = pds->get_controls();

    set<vector<string>> pop_seqs;
    make_seqs(w.size() + 1, q, dest_control, controls, pop_seqs);
    set<vector<int>> mode_seqs;
    make_mode_seqs(pds_rule, mode_seqs);

    for (auto it = pop_seqs.begin(); it != pop_seqs.end(); ++it) {
        for (auto it_modes = mode_seqs.begin(); it_modes != mode_seqs.end(); ++it_modes) {
            add_cfg_rule(pds_rule, *it, *it_modes, dest_control);
        }
    }
}



void Pds2CFG::make_seqs(int length, 
                        string const& start,
                        string const& finish,
                        set<string> const& items, 
                        set<vector<string>>& result) {
    // start with start
    result.clear();
    vector<string> init;
    init.push_back(start);
    result.insert(init);

    // wander to the end (not necessarily last mode)
    for (int i = 0; i < length - 2; i++) {
        set<vector<string>> new_seqs;
        for (auto it_seqs = result.begin(); it_seqs != result.end(); ++it_seqs) {
            for (auto it_q = items.begin(); it_q != items.end(); ++it_q) {
                vector<string> s = *it_seqs;
                s.push_back(*it_q);
                new_seqs.insert(s);
            }
        }
        result = new_seqs;
    }

    // finish at finish
    set<vector<string>> new_seqs;
    for (auto it_seqs = result.begin(); it_seqs != result.end(); ++it_seqs) {
        vector<string> s = *it_seqs;
        s.push_back(finish);
        new_seqs.insert(s);
    }
    result = new_seqs;
}

void Pds2CFG::make_mode_seqs(pds::rule_const_ptr rule, set<vector<int>>& mode_seqs) {
    int length = rule->get_w().size() + 1;
    // start with start or start + 1 (for rules that change mode)
    mode_seqs.clear();
    vector<int> init;
    init.push_back(start_mode);
    mode_seqs.insert(init);
    if ((start_mode < end_mode) &&
        (rule->get_incs().size() > 0 || rule->get_decs().size() > 0)) {
        vector<int> init2;
        init2.push_back(start_mode + 1);
        mode_seqs.insert(init2);
    }

    // wander in the middle
    for (int i = 0; i < length - 1; i++) {
        set<vector<int>> new_seqs;
        for (auto it_seqs = mode_seqs.begin(); it_seqs != mode_seqs.end(); ++it_seqs) {
            for (int i = it_seqs->back(); i <= end_mode; ++i) {
                vector<int> s = *it_seqs;
                s.push_back(i);
                new_seqs.insert(s);
            }
        }
        mode_seqs = new_seqs;
    }
}



void Pds2CFG::add_cfg_rule(pds::rule_const_ptr rule,
                           vector<string> const& pop_seq,
                           vector<int> const& mode_seq,
                           string const& dest_control) {
    int mode_out = mode_seq.back();
    int rhs_mode_in = mode_seq.front();

    string lhs = make_grammar_char(rule->get_p(),
                                   rule->get_a(),
                                   dest_control,
                                   start_mode,
                                   mode_out);

    vector<string> rhs;
    make_rhs(rule, pop_seq, mode_seq, rhs);

    add_rule_to_cfg(lhs, rhs, rule, (start_mode != rhs_mode_in));
}

void Pds2CFG::make_rhs(pds::rule_const_ptr rule,
                       vector<string> const& pop_seq,
                       vector<int> const& mode_seq,
                       vector<string>& result) {
    auto w = rule->get_w();
    assert(w.size() == pop_seq.size() - 1);
    assert(pop_seq.size() == mode_seq.size());

    add_terms(rule, mode_seq.front(), result);

    auto it_w = w.begin();
    auto it_seq = pop_seq.begin();
    auto it_modes = mode_seq.begin();
    string from = *it_seq; 
    int mode_from = *it_modes;
    ++it_seq;
    ++it_modes;
    while (it_w != w.end()) {
        string n = make_grammar_char(from, *it_w, *it_seq, mode_from, *it_modes);
        result.push_back(n);

        from = *it_seq;
        mode_from = *it_modes;
        ++it_w;
        ++it_seq;
        ++it_modes;
    }
        
}

string Pds2CFG::name_grammar_char(string const& p, 
                                  string const& a, 
                                  string const& q,
                                  int mode_in,
                                  int mode_out) {
    stringstream s_in;
    s_in << mode_in;
    stringstream s_out;
    s_out << mode_out;

    string result = "N_" + p + "_" + a + "_" + q + "_" + s_in.str() + "_" + s_out.str();
    return result;
}


string Pds2CFG::make_grammar_char(string const& p, 
                                  string const& a, 
                                  string const& q,
                                  int mode_in,
                                  int mode_out) {
    string result = name_grammar_char(p, a, q, mode_in, mode_out);

    if (mode_in == start_mode) {
        cfg->add_nonterminal(result);
    } else {
        cfg->add_terminal(result);
    }

    return result;
}


string Pds2CFG::make_term(string const& t) {
    string result = term_name(t);
    cfg->add_terminal(result);
    return result;
}

string Pds2CFG::term_name(string const& t) {
    return "t_" + t;
}

string Pds2CFG::counter_inc_term_name(string const& counter, int mode) {
    return "t_" + counter_inc_term_name_no_prefix(counter, mode);
}

string Pds2CFG::counter_dec_term_name(string const& counter, int mode) {
    return "t_" + counter_dec_term_name_no_prefix(counter, mode);
}

string Pds2CFG::counter_inc_term_name_no_prefix(string const& counter, int mode) {
    stringstream s_mode;
    s_mode << mode;
    return "+" + counter + "_" + s_mode.str();
}

string Pds2CFG::counter_dec_term_name_no_prefix(string const& counter, int mode) {
    stringstream s_mode;
    s_mode << mode;
    return "-" + counter + "_" + s_mode.str();
}

void Pds2CFG::add_terms(pds::rule_const_ptr rule, int mode, vector<string>& rhs) {
    rhs.push_back(make_term(rule->get_action()));
    stringstream s_mode;
    s_mode << mode;
    
    set<string> const& incs = rule->get_incs();
    for (auto it = incs.begin(); it != incs.end(); ++it) {
        rhs.push_back(make_term("+" + *it + "_" + s_mode.str()));
    }
    
    set<string> const& decs = rule->get_decs();
    for (auto it = decs.begin(); it != decs.end(); ++it) {
        rhs.push_back(make_term("-" + *it + "_" + s_mode.str()));
    }
}


void Pds2CFG::add_rule_to_cfg(string const& lhs,
                              vector<string> const& rhs,
                              pds::rule_const_ptr pds_rule,
                              bool change) {
    cfg::rule_ptr new_rule = boost::make_shared<cfg::Rule>(lhs, rhs);
    cfg::rule_info_ptr info = boost::make_shared<cfg::RuleInfo>();
    info->assoc_rule = pds_rule;
    info->change = change;
    new_rule->add_info((cfg::rule_info_const_ptr&)info);
    cfg->add_rule(new_rule);
}


void Pds2CFG::make_inits(string const& init_p,
                         string const& init_a,
                         string const& final_p,
                         int start_mode,
                         int end_mode,
                         set<string>& inits) {
    for (int i = start_mode; i <= end_mode; ++i) {
        inits.insert(name_grammar_char(init_p, init_a, final_p, start_mode, i));
    }
}


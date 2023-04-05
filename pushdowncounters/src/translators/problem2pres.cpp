
#include "problem2pres.h"

#include "../structures/pds.h"
#include "../structures/cfg.h"
#include "../structures/epresburger.h"
#include "../structures/problem.h"

#include "../translators/cfg2pres.h"

#include "pds2cfg.h"

#include "boost/make_shared.hpp"

#include <map>
#include <sstream>
#include <algorithm>

using namespace std;
using namespace pres;



pres_ptr Problem2pres::translateProblem(Problem const& the_prob) {
    pres_assoc_bin_ptr result(new EPConj());

    pco = &the_prob.get_pds();
    prob = &the_prob;
//    nmodes_per_counter = 3 * the_prob.get_nreversals() + 1;
    int n = the_prob.get_nreversals();
    // 2 + n + ceil(n/2)
    nmodes_per_counter = 2 + n + (n/2) + (n%2 ? 1 : 0);
    nmode_blocks = max(1, (int)(pco->get_counters().size() * nmodes_per_counter));

    set_up_variables();
    pres_ptr start = make_start();
    result->add_clause(start);
    pres_ptr sequence = make_sequence();
    result->add_clause(sequence);
    pres_ptr run_clauses =  make_run_clauses();
    result->add_clause(run_clauses);
    pres_ptr valid_clauses = make_valid_clauses();
    result->add_clause(valid_clauses);
    pres_ptr connects_clauses = make_connects_clauses();
    result->add_clause(connects_clauses);
    pres_ptr good_ops = make_good_counter_ops();
    result->add_clause(good_ops);
    pres_ptr user_cons = make_user_constraint();
    result->add_clause(user_cons);
    pres_ptr final_result = quantify((pres_ptr&)result);

    delete_variables();

    return final_result;
}


void Problem2pres::set_up_variables() {
    Pds2CFG translator;
    mode_cfgs = new cfg::cfg_ptr[nmode_blocks];
    nonterms_in = new map<string, pres_val_ptr>[nmode_blocks];
    mode_vars = new map<string, pres_var_ptr>[nmode_blocks];
    counter_inc_counts = new map<string, pres_val_ptr>[nmode_blocks];
    counter_dec_counts = new map<string, pres_val_ptr>[nmode_blocks];
    rule_vars.clear();

    mode_consts = new pres_val_ptr[nmodes_per_counter];
    for (int i = 0; i < nmodes_per_counter; ++i) {
        mode_consts[i] = boost::make_shared<EPInteger>(i);
    }

    string const& init_p = prob->get_init_control();
    string const& init_a = prob->get_init_char();
    string const& final_p = prob->get_final_control();
    Pds2CFG::make_inits(init_p, init_a, final_p, 0, nmode_blocks-1, inits);

    for (int i = 0; i < nmode_blocks; ++i) {
        mode_cfgs[i] = translator.translatePds(*pco, i, nmode_blocks-1);
    }
    minimise_mode_cfgs();

    for (int i = 0; i < nmode_blocks; ++i) {
        add_rules_vars(mode_cfgs[i], i);
        make_var_map(i, pco->get_counters(), mode_vars[i]);
    }

    for (int i = 0; i < nmode_blocks; ++i) {
        cfg::cfg_const_ptr cfgi = mode_cfgs[i];
        make_counter_inc_counts(i, pco->get_counters(), counter_inc_counts[i]);  
        make_counter_dec_counts(i, pco->get_counters(), counter_dec_counts[i]);  
        make_nonterms_in_map(i, cfgi->get_nonterminals(), nonterms_in[i]);
    }

    make_terms_out_map(pco->get_actions(), terms_out);
}

void Problem2pres::delete_variables() {
    rule_vars.clear();
    zero_checks.clear();
    up_checks.clear();
    down_checks.clear();
    delete [] mode_cfgs;
    delete [] nonterms_in;
    delete [] mode_vars;
    delete [] mode_consts;
    delete [] counter_inc_counts;
    delete [] counter_dec_counts;
}


void Problem2pres::make_counter_inc_counts(int i, 
                                           set<string> const& counters, 
                                           map<string, pres_val_ptr>& map) {
    // nonterm_in = number produced by fired rules for all earlier grammars
    for (auto it = counters.begin(); it != counters.end(); ++it) {
        pres_plus_ptr amount = boost::make_shared<EPPlus>();
        for (int j = 0; j <= i; j++) {
            string inc = Pds2CFG::counter_inc_term_name(*it, j);
            // previous mode_cfg may also produce the term as the mode change
            // rule
            if (j > 0) {
                set<string> const& termsj = mode_cfgs[j-1]->get_terminals();
                auto has = termsj.find(inc);
                if (has != termsj.end()) {
                    add_num_terms_produced(inc, j-1, amount);
                }
            }
            set<string> const& termsj = mode_cfgs[j]->get_terminals();
            auto has = termsj.find(inc);
            if (has != termsj.end()) {
                add_num_terms_produced(inc, j, amount);
            }
        }
        map.insert(make_pair(*it, amount));
    }
}

void Problem2pres::make_counter_dec_counts(int i, 
                                           set<string> const& counters, 
                                           map<string, pres_val_ptr>& map) {
    // nonterm_in = number produced by fired rules for all earlier grammars
    for (auto it = counters.begin(); it != counters.end(); ++it) {
        pres_plus_ptr amount = boost::make_shared<EPPlus>();
        for (int j = 0; j <= i; j++) {
            string dec = Pds2CFG::counter_dec_term_name(*it, j);
            // previous mode_cfg may also produce the term as the mode change
            // rule
            if (j > 0) {
                set<string> const& termsj = mode_cfgs[j-1]->get_terminals();
                auto has = termsj.find(dec);
                if (has != termsj.end()) {
                    add_num_terms_produced(dec, j-1, amount);
                }
            }
            set<string> const& termsj = mode_cfgs[j]->get_terminals();
            auto has = termsj.find(dec);
            if (has != termsj.end()) {
                add_num_terms_produced(dec, j, amount);
            }
        }
        map.insert(make_pair(*it, amount));
    }
}

void Problem2pres::make_terms_out_map(set<string> const& terms, 
                                      map<string, pres_val_ptr>& map) {
    // term_out = number produced by fired rules for all grammars
    for (auto it = terms.begin(); it != terms.end(); ++it) {
        pres_plus_ptr amount = boost::make_shared<EPPlus>();
        for (int j = 0; j < nmode_blocks; j++) {
            string term_name = Pds2CFG::term_name(*it);
            set<string> const& termsj = mode_cfgs[j]->get_terminals();
            if (termsj.find(term_name) != termsj.end()) {
                add_num_terms_produced(term_name, j, amount);
            }
        }
        map.insert(make_pair(*it, amount));
    }
}



void Problem2pres::make_nonterms_in_map(int i, 
                                        set<string> const& nonterms, 
                                        map<string, pres_val_ptr>& map) {
    if (i == 0) {
        for (auto it = nonterms.begin(); it != nonterms.end(); ++it) {
            if (inits.find(*it) != inits.end()) {
                // var if init
                map.insert(make_pair(*it, make_var(*it, i)));
            } else {
                // zero ow.
                map.insert(make_pair(*it, mode_consts[0]));
            }
        }
    } else {
        // nonterm_in = number produced by fired rules for all earlier grammars
        for (auto it = nonterms.begin(); it != nonterms.end(); ++it) {
            pres_plus_ptr amount = boost::make_shared<EPPlus>();
            for (int j = 0; j < i; j++) {
                add_num_terms_produced(*it, j, amount);
            }
            map.insert(make_pair(*it, amount));
        }
    }
}


void Problem2pres::add_num_terms_produced(string const& nonterm, int i, pres_plus_ptr& amount) {
    set<cfg::rule_const_ptr> const& rules = mode_cfgs[i]->rules_by_body(nonterm);
    for (auto it = rules.begin(); it != rules.end(); ++it) {
        auto rule = *it;
        pres_var_ptr rule_var = get_rule_var(rule);

        int num_rhs = rule->count_in_body(nonterm);
        if (num_rhs == 1) {
            amount->add_operand(rule_var);
        } else if (num_rhs > 1) {
            pres_val_ptr clause = boost::make_shared<EPConstMult>(num_rhs, rule_var);
            amount->add_operand(clause);
        }
    }
}



pres_ptr Problem2pres::make_sequence() {
    pres_assoc_bin_ptr seq = boost::make_shared<EPConj>();
    set<string> const& counters = pco->get_counters();
    for (auto it = counters.begin(); it != counters.end(); ++it) {
        pres_var_ptr init_m = get_mode_var(*it, 0);
        pres_ptr eqzero = boost::make_shared<EPEqual>((pres_val_ptr&)init_m,
                                                      mode_consts[0]);
        seq->add_clause(eqzero);

        for (int i = 0; i < nmode_blocks; ++i) {
            pres_var_ptr m = get_mode_var(*it, i);

            if (i > 0) {
                pres_ptr ltmax = boost::make_shared<EPLessThanEq>((pres_val_ptr&)m,
                                                                  mode_consts[nmodes_per_counter-1]);
                seq->add_clause(ltmax);
            }
            
            if (i < nmode_blocks - 1) {
                pres_var_ptr m_next = get_mode_var(*it, i+1);
                pres_ptr nextgte = boost::make_shared<EPLessThanEq>((pres_val_ptr&)m,
                                                                    (pres_val_ptr&)m_next);
                seq->add_clause(nextgte);
            }
        }
    }
    return seq;
}


void Problem2pres::add_rules_vars(cfg::cfg_const_ptr cfg, int i) {
    int nrules = 0;
    vector<cfg::rule_const_ptr> const& rules = cfg->get_rules();
    stringstream s_i;
    s_i << i;

//    //cout << "cfg " << i << " has " << cfg->get_rules().size() << " rules." << endl;
//    //cout << "and is " << *cfg << endl;

    for (auto it = rules.begin(); it != rules.end(); ++it) {
        stringstream rule_num;
        rule_num << nrules++;
        string var = "m" + s_i.str() + "_R" + rule_num.str();
        rule_vars.insert(make_pair(*it, boost::make_shared<EPVar>(var)));
    }
}


pres_ptr Problem2pres::make_run_clauses() {
    pres_assoc_bin_ptr clauses(new EPConj());

    for (int i = 0; i < nmode_blocks; ++i) {
        pres_ptr runi = make_run_clause(i);
        clauses->add_clause(runi);
    }

    return clauses;
}


pres_ptr Problem2pres::make_run_clause(int i) {
    CFG2Pres translator;
    stringstream s_i;
    s_i << i;
    string new_var_prefix("m" + s_i.str() + "_");
    return translator.translateCFG(*mode_cfgs[i], 
                                   nonterms_in[i], 
                                   rule_vars, 
                                   new_var_prefix);
}


template <class V>
void Problem2pres::make_var_map(int i, set<string> const& chars, map<string, V>& map) {
    for (auto it = chars.begin(); it != chars.end(); ++it) {
        map.insert(make_pair(*it, make_var(*it, i)));
    }
}


pres_var_ptr Problem2pres::make_var(string const& ch, int i) {
    stringstream s_i;
    s_i << i;
    string name = "m" + s_i.str() + "_" + ch;
    return boost::make_shared<EPVar>(name);
}




pres_ptr Problem2pres::make_valid_clauses() {
    pres_assoc_bin_ptr clauses = boost::make_shared<EPConj>();

    for (int i = 0; i < nmode_blocks; ++i) {
        pres_ptr runi = make_valid_clause(i);
        clauses->add_clause(runi);
    }

    return clauses;
}


pres_ptr Problem2pres::make_valid_clause(int i) {
    pres_assoc_bin_ptr clause = boost::make_shared<EPConj>();

    vector<cfg::rule_const_ptr> const& rulesi = mode_cfgs[i]->get_rules();
    for (auto it = rulesi.begin(); it != rulesi.end(); ++it) {
        pres_var_ptr rule_var = get_rule_var(*it);
        pres_ptr gtzero = boost::make_shared<EPLessThan>(mode_consts[0], 
                                                         (pres_val_ptr&)rule_var);
        pres_ptr applicable = make_applicable(*it, i);
        pres_ptr implies = boost::make_shared<EPImplies>(gtzero, applicable);
        clause->add_clause(implies);
    }

    return clause;
}


pres_ptr Problem2pres::make_applicable(cfg::rule_const_ptr rule, int i) {
    pres_ptr p;
    
    cfg::rule_info_const_ptr info = rule->get_info();
    bool change = info->change;
    pds::rule_const_ptr pds_rule = info->assoc_rule;
    set<string> const& incs = pds_rule->get_incs();
    set<string> const& decs = pds_rule->get_decs();

    pres_ptr guard = make_rule_guard(pds_rule->get_guard(), i);
    
    if ((incs.size() == 0) && (decs.size() == 0)) {
        if (!change) {
            p = guard;
        } else {
            p = boost::make_shared<EPBool>(0);
        }
    } else {
        pres_assoc_bin_ptr tests = boost::make_shared<EPConj>();
        if (change) {
            for (auto it = incs.begin(); it != incs.end(); ++it) {
                pres_var_ptr var = get_mode_var(*it, i);
                pres_ptr zero = make_zero_check(var);
                pres_ptr down = make_down_check(var);
                tests->add_clause(boost::make_shared<EPDisj>(zero, down));
            }
            for (auto it = decs.begin(); it != decs.end(); ++it) {
                pres_var_ptr var = get_mode_var(*it, i);
                pres_ptr up = make_up_check(var);
                pres_ptr down = make_down_check(var);
                tests->add_clause(boost::make_shared<EPDisj>(up, down));
            }
        } else {
            for (auto it = incs.begin(); it != incs.end(); ++it) {
                pres_var_ptr var = get_mode_var(*it, i);
                pres_ptr up = make_up_check(var);
                tests->add_clause(up);
            }
            for (auto it = decs.begin(); it != decs.end(); ++it) {
                pres_var_ptr var = get_mode_var(*it, i);
                pres_ptr down = make_down_check(var);
                tests->add_clause(down);
            }
        }
        tests->add_clause(guard);
        p = tests;
    }

    return p;
}


pres_ptr Problem2pres::make_zero_check(pres_var_ptr& var) {
    return make_check(var, CHECK_ZERO_OFFSET, zero_checks);
}

pres_ptr Problem2pres::make_up_check(pres_var_ptr& var) {
    return make_check(var, CHECK_UP_OFFSET, up_checks);
}

pres_ptr Problem2pres::make_down_check(pres_var_ptr& var) {
    return make_check(var, CHECK_DOWN_OFFSET, down_checks);
}

pres_ptr Problem2pres::make_check(pres_var_ptr& var, 
                              int offset, 
                              map<pres_var_ptr, pres_ptr>& memo) {
    auto result = memo.find(var);
    pres_ptr fmla;
    if (result == memo.end()) {
        pres_assoc_bin_ptr check = boost::make_shared<EPDisj>();
        for (int i = offset; i < nmodes_per_counter; i += MODE_CYCLE) {
            pres_ptr eq = boost::make_shared<EPEqual>((pres_val_ptr&)var, mode_consts[i]);
            check->add_clause(eq);
        }
        memo.insert(make_pair(var, check));
        fmla = check;
    } else {
        fmla = result->second;
    }
    return fmla;
}


pres_var_ptr Problem2pres::get_rule_var(cfg::rule_const_ptr rule) {
    auto v = rule_vars.find(rule);
    if (v == rule_vars.end()) {
        cerr << "Problem2pres::get_rule_var got a rule it didn't understand:" << endl;
        cerr << *rule << endl;
        exit(-1);
    }
    return v->second;
}


pres_var_ptr Problem2pres::get_mode_var(string const& counter, int i) {
    auto result = mode_vars[i].find(counter);
    if (result == mode_vars[i].end()) {
        cerr << "Problem2pres::get_mode_var asked to look up bad counter " << endl;
        cerr << counter << endl;
        exit(-1);
    }
    return result->second;
}





pres_ptr Problem2pres::make_rule_guard(pds::rule_guard_ptr guard, int i) {
    class RGTransformer : public pds::RuleGuardVisitor {
        pres_ptr result;
        int i;
        Problem2pres& container;

        public: 
            RGTransformer(int i_val, Problem2pres& new_con) 
                : i(i_val),
                  container(new_con) {}

            pres_ptr transform(pds::rule_guard_ptr fmla) {
                fmla->accept(*this);
                return result;
            }

            virtual void visit(pds::RGBool& f) {
                result = boost::make_shared<EPBool>(f.get_value());
            }

            virtual void visit(pds::RGNonZero& f) {
                set<string> const& counters = container.pco->get_counters();
                string const& counter = f.get_counter();
                auto has_counter = counters.find(counter);
                if (has_counter != counters.end()) {
                    pres_var_ptr var = container.get_mode_var(f.get_counter(), i);
                    pres_ptr zero = container.make_zero_check(var);
                    result = boost::make_shared<EPNeg>(zero);
                } else {
                    // a counter not incremented can never be non-zero
                    result = boost::make_shared<EPBool>(0);
                }
            }

            virtual void visit(pds::RGZero& f) {
                set<string> const& counters = container.pco->get_counters();
                string const& counter = f.get_counter();
                auto has_counter = counters.find(counter);
                if (has_counter != counters.end()) {
                    pres_var_ptr var = container.get_mode_var(f.get_counter(), i);
                    result = container.make_zero_check(var);
                } else {
                    // a counter not incremented can never be non-zero
                    result = boost::make_shared<EPBool>(1);
                }
            }

            virtual void visit(pds::RGNeg& f) {
                pres_ptr p = this->transform(f.get_fmla());
                result = boost::make_shared<EPNeg>(p);
            }

            virtual void visit(pds::RGImplies& f) {
                pres_ptr p1 = this->transform(f.get_lhs());
                pres_ptr p2 = this->transform(f.get_rhs());
                result = boost::make_shared<EPImplies>(p1, p2);
            }

            virtual void visit(pds::RGDisj& f) {
                pres_assoc_bin_ptr res = boost::make_shared<EPDisj>();
                vector<pds::rule_guard_ptr> const& clauses = f.get_clauses();

                for (auto it = clauses.begin(); it != clauses.end(); ++it)  {
                    res->add_clause(this->transform(*it));
                }

                result = res;
            }

            virtual void visit(pds::RGConj& f) {
                pres_assoc_bin_ptr res = boost::make_shared<EPConj>();
                vector<pds::rule_guard_ptr> const& clauses = f.get_clauses();

                for (auto it = clauses.begin(); it != clauses.end(); ++it)  {
                    res->add_clause(this->transform(*it));
                }

                result = res;
            }
    };

    pres_ptr res;
    if (guard) {
        RGTransformer trans(i, *this);
        res = trans.transform(guard);
    } else {
        res = boost::make_shared<EPBool>(1);
    }
    return res;
}




pres_ptr Problem2pres::make_connects_clauses() {
    pres_assoc_bin_ptr result = boost::make_shared<EPConj>();
    for (int i = 0; i < nmode_blocks - 1; ++i) {
        result->add_clause(make_connects_clause(i));    
    }
    return result;
}


pres_ptr Problem2pres::make_connects_clause(int i) {
    pres_assoc_bin_ptr result = boost::make_shared<EPConj>();
    pres_assoc_bin_ptr allzeros = boost::make_shared<EPConj>();

    // we change (good change rule fired)
    vector<cfg::rule_const_ptr> const& rulesi = mode_cfgs[i]->get_rules();
    for (auto it = rulesi.begin(); it != rulesi.end(); ++it) {
        auto info = (*it)->get_info();
        if (info->change) {
            auto rule_var = get_rule_var(*it);
            pres_ptr gtzero = boost::make_shared<EPLessThan>(mode_consts[0], 
                                                             (pres_val_ptr&)rule_var);
            pres_ptr eqzero = boost::make_shared<EPEqual>(mode_consts[0], 
                                                          (pres_val_ptr&)rule_var);
            pres_ptr connects = make_rule_connect(*it, i);
            result->add_clause(boost::make_shared<EPImplies>(gtzero, connects));
            allzeros->add_clause(eqzero);
        }
    }

    // or we are equal (no mode change rules fired)
    pres_assoc_bin_ptr nochange = boost::make_shared<EPConj>();
    set<string> const& counters = pco->get_counters();
    for (auto it = counters.begin(); it != counters.end(); ++it) {
        pres_var_ptr m1 = get_mode_var(*it, i);
        pres_var_ptr m2 = get_mode_var(*it, i+1);
        pres_ptr eq = boost::make_shared<EPEqual>((pres_val_ptr&)m1,
                                                  (pres_val_ptr&)m2);
        nochange->add_clause(eq);
    }

    pres_ptr last = boost::make_shared<EPImplies>((pres_ptr&)allzeros,
                                                  (pres_ptr&)nochange);
    result->add_clause(last);

    return result;
}



pres_ptr Problem2pres::make_rule_connect(cfg::rule_const_ptr rule, int i) {
    pres_ptr p;
    
    cfg::rule_info_const_ptr info = rule->get_info();
    bool change = info->change;
    pds::rule_const_ptr pds_rule = info->assoc_rule;
    set<string> const& incs = pds_rule->get_incs();
    set<string> const& decs = pds_rule->get_decs();

    pres_ptr guard = make_rule_guard(pds_rule->get_guard(), i);
  
    // else not a good rule for connecting modes...
    assert(change);
    if ((incs.size() == 0) && (decs.size() == 0)) {
        p = boost::make_shared<EPBool>(0);
    } else {
        pres_assoc_bin_ptr tests = boost::make_shared<EPConj>();
        for (auto it = incs.begin(); it != incs.end(); ++it) {
            pres_var_ptr var_in = get_mode_var(*it, i);
            pres_var_ptr var_out = get_mode_var(*it, i+1);
            pres_ptr zero = make_zero_check(var_in);
            pres_ptr down = make_down_check(var_in);
            pres_ptr up = make_up_check(var_out);
            pres_ptr disj = boost::make_shared<EPDisj>(zero, down);
            pres_ptr conj = boost::make_shared<EPConj>(disj, up);
            tests->add_clause(conj);
        }
        for (auto it = decs.begin(); it != decs.end(); ++it) {
            pres_var_ptr var_in = get_mode_var(*it, i);
            pres_var_ptr var_out = get_mode_var(*it, i+1);
            pres_ptr up = make_up_check(var_in);
            pres_ptr down = make_down_check(var_in);
            pres_ptr down_out = make_down_check(var_out);
            pres_ptr zero_out = make_zero_check(var_out);
            pres_ptr choice1 = boost::make_shared<EPConj>(up, down_out);
            pres_ptr choice2 = boost::make_shared<EPConj>(up, zero_out);
            pres_ptr choice3 = boost::make_shared<EPConj>(down, zero_out);
            pres_assoc_bin_ptr all = boost::make_shared<EPDisj>();
            all->add_clause(choice1);
            all->add_clause(choice2);
            all->add_clause(choice3);
            tests->add_clause((pres_ptr&)all);
        }
        p = tests;
    }

    return p;
}





pres_val_ptr Problem2pres::get_nonterm_val(string const& nonterm, int i) {
    return safe_lookup(nonterm, nonterms_in[i]);
}

pres_val_ptr Problem2pres::get_term_val(string const& term) {
    return safe_lookup(term, terms_out);
}

pres_val_ptr Problem2pres::get_counter_inc_counts(string const& counter, int i) {
    return safe_lookup(counter, counter_inc_counts[i]);
}

pres_val_ptr Problem2pres::get_counter_dec_counts(string const& counter, int i) {
    return safe_lookup(counter, counter_dec_counts[i]);
}



template <class T, class U> 
U Problem2pres::safe_lookup(T const& key, map<T, U>& map) {
    auto result = map.find(key);
    if (result == map.end()) {
        cerr << "Problem2pres::safe_lookup could not find " 
             << key
             << " in map" << endl;
        exit(-1);
    }
    return result->second;
}


pres_ptr Problem2pres::make_good_counter_ops() {
    pres_assoc_bin_ptr result = boost::make_shared<EPConj>();
    set<string> const& counters = pco->get_counters();

    for (int i = 0; i < nmode_blocks; ++i) {
        for (auto it = counters.begin(); it != counters.end(); ++it) {
            pres_val_ptr sum_incs = get_counter_inc_counts(*it, i);
            pres_val_ptr sum_decs = get_counter_dec_counts(*it, i);

            pres_var_ptr m = get_mode_var(*it, i);
           
            pres_ptr m_zero = make_zero_check(m);
            pres_ptr m_nzero = boost::make_shared<EPNeg>(m_zero);

            pres_ptr eq = boost::make_shared<EPEqual>(sum_incs, sum_decs);
            pres_ptr gt = boost::make_shared<EPLessThan>(sum_decs, sum_incs);

            pres_ptr imp1 = boost::make_shared<EPImplies>(m_zero, eq);
            pres_ptr imp2 = boost::make_shared<EPImplies>(m_nzero, gt);

            result->add_clause(imp1);
            result->add_clause(imp2);
        }
    }
    
    return result;
}


pres_ptr Problem2pres::make_start() {
    pres_plus_ptr sum_inits = boost::make_shared<EPPlus>();

    set<string> const& nonterms = mode_cfgs[0]->get_nonterminals();
    for (auto it = nonterms.begin(); it != nonterms.end(); ++it) {
        pres_val_ptr v = get_nonterm_val(*it, 0);
        if (inits.find(*it) != inits.end()) {
            sum_inits->add_operand(v);
        }
    }

    pres_ptr is_one = boost::make_shared<EPEqual>((pres_val_ptr&)sum_inits, mode_consts[1]);
    return is_one;
}




void Problem2pres::minimise_mode_cfgs() {
    mode_cfgs[0]->minimise_cfg(inits);

    set<string> mode_inits = inits;
    for (int i = 1; i < nmode_blocks; ++i) {
        set<string> const& prev_terms = mode_cfgs[i-1]->get_terminals();
        for (auto it = prev_terms.begin(); it != prev_terms.end(); ++it) {
            mode_inits.insert(*it);
        }
        mode_cfgs[i]->minimise_cfg(mode_inits);
    }
}


pres_ptr Problem2pres::quantify(pres_ptr& fmla) {
    pres_exists_ptr efmla = boost::make_shared<EPExists>(fmla);

    for (auto it = inits.begin(); it != inits.end(); ++it) {
        pres_var_ptr v = make_var(*it, 0);
        efmla->add_var(v);
    }
 
    quantify_map_vars(efmla, rule_vars);

    for (int i = 0; i < nmode_blocks; ++i) {
        quantify_map_vars(efmla, mode_vars[i]);
    }
 
    return efmla;
}

template<class T>
void Problem2pres::quantify_map_vars(pres_exists_ptr& efmla, map<T, pres_var_ptr>& map) {
    for (auto it = map.begin(); it != map.end(); ++it) {
        efmla->add_var(it->second);
    }
}




pres_ptr Problem2pres::make_user_constraint() {
    class EPTransformer : public EPresburgerVisitor, public EPValVisitor {
        pres_ptr result;
        pres_val_ptr result_val;
        Problem2pres& container;
        set<string> quant_vars;

        public: 
            EPTransformer(Problem2pres& new_con) 
                : container(new_con) {}

            pres_ptr transform(pres_ptr fmla) {
                recurse(fmla);
                return result;
            }

            virtual void visit(EPLessThan& obj){
                recurse(obj.get_lhs());
                pres_val_ptr lhs = result_val;
                recurse(obj.get_rhs());
                result = boost::make_shared<EPLessThan>(lhs, result_val);
            }

            virtual void visit(EPLessThanEq& obj){
                recurse(obj.get_lhs());
                pres_val_ptr lhs = result_val;
                recurse(obj.get_rhs());
                result = boost::make_shared<EPLessThanEq>(lhs, result_val);
            }

            virtual void visit(EPEqual& obj){
                recurse(obj.get_lhs());
                pres_val_ptr lhs = result_val;
                recurse(obj.get_rhs());
                result = boost::make_shared<EPEqual>(lhs, result_val);
            }

            virtual void visit(EPImplies& obj){
                recurse(obj.get_lhs());
                pres_ptr lhs = result;
                recurse(obj.get_rhs());
                result = boost::make_shared<EPImplies>(lhs, result);
            }

            virtual void visit(EPConj& obj){
                pres_assoc_bin_ptr conj = boost::make_shared<EPConj>();
                do_assoc_bin(obj, conj);
                result = conj;
            }

            virtual void visit(EPDisj& obj){
                pres_assoc_bin_ptr disj = boost::make_shared<EPDisj>();
                do_assoc_bin(obj, disj);
                result = disj;
            }

            virtual void visit(EPNeg& obj){
                recurse(obj.get_fmla());
                result = boost::make_shared<EPNeg>(result);
            }

            virtual void visit(EPExists& obj){
                auto old_vars = quant_vars;
                auto vars = obj.get_vars();
                for (auto it = vars.begin(); it != vars.end(); ++it) {
                    quant_vars.insert(*it);
                }
                recurse(obj.get_fmla());
                result = boost::make_shared<EPExists>(vars, result);
                quant_vars = old_vars;
            }

            virtual void visit(EPBool& obj){
                result = boost::make_shared<EPBool>(obj.get_value());
            }

            virtual void visit(EPVar& obj){
                result_val = lookup_var(obj);
            }

            virtual void visit(EPPlus& obj){
                pres_plus_ptr sum = boost::make_shared<EPPlus>();
                vector<pres_val_ptr> const& opers = obj.get_operands();
                for (auto it = opers.begin(); it != opers.end(); ++it) {
                    recurse(*it);
                    sum->add_operand(result_val);
                }
                result_val = sum;
            }

            virtual void visit(EPConstMult& obj){
                recurse(obj.get_scalee());
                int scalar = obj.get_scalar();
                result_val = boost::make_shared<EPConstMult>(scalar, result_val);
            }

            virtual void visit(EPInteger& obj){
                result_val = boost::make_shared<EPInteger>(obj.get_value());
            }

            virtual void visit(EPMinus& obj){
                recurse(obj.get_lhs());
                pres_val_ptr lhs = result_val;
                recurse(obj.get_rhs());
                result_val = boost::make_shared<EPMinus>(lhs, result_val);
            }

        private:
            void recurse(pres_ptr f) {
                f->accept(*this);
            }

            void recurse(pres_val_ptr f) {
                f->accept(*this);
            }

            void do_assoc_bin(EPAssocBin const& obj, pres_assoc_bin_ptr& res) {
                vector<pres_ptr> const& clauses = obj.get_clauses();
                for (auto it = clauses.begin(); it != clauses.end(); ++it) {
                    recurse(*it);
                    res->add_clause(result);
                }
            }

            pres_val_ptr lookup_var(EPVar& var) {
                string name = var.get_name();
                pres_val_ptr result;
                auto it = quant_vars.begin();
                bool found = 0;
                while(!found && (it != quant_vars.end())) {
                    found = (name == *it);
                    if (found)
                        result = boost::make_shared<EPVar>(*it);
                    else
                        ++it;
                }
                if (!found) {
                    set<string> const& actions = container.pco->get_actions();
                    set<string> const& counters = container.pco->get_counters();
                    if (actions.find(name) != actions.end()) {
                        result = container.get_term_val(name);
                    } else if (counters.find(name) != counters.end()) {
                        int last = container.nmode_blocks - 1;
                        pres_val_ptr incs = container.get_counter_inc_counts(name, last);
                        pres_val_ptr decs = container.get_counter_dec_counts(name, last);
                        result = boost::make_shared<EPMinus>(incs, decs);
                    } else {
                        cerr << "Constraint contains variable " 
                             << name 
                             << ", which is neither quantified, a counter or an action." 
                             << endl;
                        exit(-1);
                    }
                }

                return result;
            }
    };

    pres_ptr res;
    EPTransformer trans(*this);
    res = trans.transform(prob->get_constraint());

    return res;
}




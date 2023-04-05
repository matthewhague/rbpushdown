
#include "cfg2pres.h"

#include <vector>
#include <set>
#include <iostream>
#include <string>
#include <sstream>

#include "boost/make_shared.hpp"

using namespace std;
using namespace pres;


pres_ptr CFG2Pres::translateCFG(cfg::CFG const& tran_cfg,
                                map<string, pres_val_ptr> const& nonterms_in,
                                map<cfg::rule_const_ptr, pres_var_ptr> const& rules_out,
                                string const& new_var_prefix) {
    cfg = &tran_cfg;
    nonterm_vals = &nonterms_in;
    rules_vars = &rules_out;
    var_prefix = &new_var_prefix;


    // Now, following Verma et al, there are three main parts
    pres_assoc_bin_ptr fmla = boost::make_shared<EPConj>();

    // first bullet in paper
    pres_ptr clause_counts = make_counts_clause();
    // second bullet omitted (use rules_out info instead)
    // third bullet below
    pres_ptr clause_connects = make_connects_clause();

    fmla->add_clause(clause_counts);
    fmla->add_clause(clause_connects);

    return fmla;
}



pres_ptr CFG2Pres::make_counts_clause() {
    pres_assoc_bin_ptr clause = boost::make_shared<EPConj>();
    set<string> const& nonterms = cfg->get_nonterminals();
    for(auto it = nonterms.begin(); it != nonterms.end(); ++it) {
        pres_ptr nt_clause = make_nonterm_count_clause(*it);
        clause->add_clause(nt_clause);
    }
    return clause;
}


pres_ptr CFG2Pres::make_nonterm_count_clause(string const& nonterm) {
    // we want
    //   Init_num(nt) + Sum(#nt generated by fired rules) =
    //   Sum(rules fired with nt on lhs)

    pres_plus_ptr lhs = boost::make_shared<EPPlus>();
    pres_plus_ptr rhs = boost::make_shared<EPPlus>();

    pres_val_ptr init = get_nonterm_val(nonterm);
    lhs->add_operand(init);

    set<cfg::rule_const_ptr> const& body_rules = cfg->rules_by_body(nonterm);
    for (auto it = body_rules.begin(); it != body_rules.end(); ++it) {
        auto rule = *it;
        pres_var_ptr rule_var = get_rule_var(rule);

        int num_rhs = rule->count_in_body(nonterm);
        if (num_rhs == 1) {
            lhs->add_operand(rule_var);
        } else if (num_rhs > 1) {
            pres_val_ptr clause = boost::make_shared<EPConstMult>(num_rhs, rule_var);
            lhs->add_operand(clause);
        }
    }

    set<cfg::rule_const_ptr> const& head_rules = cfg->rules_by_head(nonterm);
    for (auto it = head_rules.begin(); it != head_rules.end(); ++it) {
        pres_var_ptr rule_var = get_rule_var(*it);
        rhs->add_operand(rule_var);
    }

    pres_ptr eq = boost::make_shared<EPEqual>((pres_val_ptr&)lhs,  
                                              (pres_val_ptr&)rhs);
    return eq;
}


pres_ptr CFG2Pres::make_not_generated(string const& character) {
    pres_val_ptr zero = boost::make_shared<EPInteger>(0);
    pres_assoc_bin_ptr not_generated = boost::make_shared<EPConj>();
    set<cfg::rule_const_ptr> const& body_rules = cfg->rules_by_body(character);
    for (auto it_rules = body_rules.begin(); it_rules != body_rules.end(); ++it_rules) {
        auto rule = *it_rules;
        pres_val_ptr var_rule = get_rule_var(rule);
        pres_ptr rule_eq_zero = boost::make_shared<EPEqual>(zero, var_rule);
        not_generated->add_clause(rule_eq_zero);
    }
    return not_generated;
}


void CFG2Pres::make_first_bullet(pres_assoc_bin_ptr clause,
                                 map<string, pres_var_ptr> const& zs) {
    pres_val_ptr zero = boost::make_shared<EPInteger>(0);
    pres_val_ptr one = boost::make_shared<EPInteger>(1);

    set<string> const& nonterms = cfg->get_nonterminals();
    set<string> const& terms = cfg->get_terminals();

    // (X_A = 0 or Z_A > 0)
    for (auto it = terms.begin(); it != terms.end(); ++it) {
        pres_assoc_bin_ptr disj = boost::make_shared<EPDisj>();
        pres_ptr not_generated = make_not_generated(*it);

        pres_val_ptr z = safe_lookup(*it, &zs);
        pres_ptr gtzero = boost::make_shared<EPLessThan>(zero, z);

        disj->add_clause(not_generated);
        disj->add_clause(gtzero);
        clause->add_clause(disj);
    }

    // extra sub-bullet for missing clauses 
    // (X_A = 0 or A is init or Z_A > 0)
    for (auto it = nonterms.begin(); it != nonterms.end(); ++it) {
        pres_assoc_bin_ptr disj = boost::make_shared<EPDisj>();
        pres_ptr not_generated = make_not_generated(*it);

        pres_val_ptr in_head = get_nonterm_val(*it);
        pres_ptr is_init = boost::make_shared<EPLessThan>(zero, in_head);

        pres_val_ptr z = safe_lookup(*it, &zs);
        pres_ptr gtzero = boost::make_shared<EPLessThan>(zero, z);

        disj->add_clause(not_generated);
        disj->add_clause(is_init);
        disj->add_clause(gtzero);
        clause->add_clause(disj);
    }
}

//    for (auto it = terms.begin(); it != terms.end(); ++it) {
//        pres_assoc_bin_ptr disj = boost::make_shared<EPDisj>();
//        pres_assoc_bin_ptr eqzero = boost::make_shared<EPConj>();
//        // x_A = 0 iff no fired rule produces it
//        set<cfg::rule_const_ptr> const& prod_rules = cfg->rules_by_body(*it);
//        for (auto prod_it = prod_rules.begin(); prod_it != prod_rules.end(); ++prod_it) {
//            pres_val_ptr rule_var = get_rule_var(*prod_it);
//            pres_ptr rule_zero = boost::make_shared<EPEqual>(rule_var, zero);
//            eqzero->add_clause(rule_zero);
//        }
//        pres_val_ptr z = safe_lookup(*it, &zs);
//        pres_ptr gtzero = boost::make_shared<EPLessThan>(zero, z);
//        disj->add_clause(eqzero);
//        disj->add_clause(gtzero);
//        clause->add_clause(disj);
//    }

void CFG2Pres::make_second_bullet(pres_assoc_bin_ptr clause,
                                  map<string, pres_var_ptr> const& zs) {
    pres_val_ptr zero = boost::make_shared<EPInteger>(0);
    pres_val_ptr one = boost::make_shared<EPInteger>(1);

    set<string> const& nonterms = cfg->get_nonterminals();
    set<string> const& terms = cfg->get_terminals();

    for (auto it = terms.begin(); it != terms.end(); ++it) {
        pres_val_ptr z = safe_lookup(*it, &zs);
        make_second_bullet_big_disjunct(*it, zs, zero, one, z, clause);
    }

    for (auto it = nonterms.begin(); it != nonterms.end(); ++it) {
        pres_val_ptr z = safe_lookup(*it, &zs);
        make_second_bullet_big_disjunct(*it, zs, zero, one, z, clause);
    }
}

pres_ptr CFG2Pres::make_connects_clause() {
    // not obvious, should look at paper for what this nonsense is...
    map<string, pres_var_ptr> zs;
    string zt_((*var_prefix) + "zt_");
    string zn_((*var_prefix) + "zn_");

    make_nonterm_vars(*cfg, zn_, zs);
    make_term_vars(*cfg, zt_, zs);

    pres_assoc_bin_ptr clause = boost::make_shared<EPConj>();

    pres_val_ptr zero = boost::make_shared<EPInteger>(0);
    pres_val_ptr one = boost::make_shared<EPInteger>(1);

    set<string> const& nonterms = cfg->get_nonterminals();
    set<string> const& terms = cfg->get_terminals();


//    make_second_bullet(clause, zs);
//    make_first_bullet(clause, zs);

    // (X_A = 0 or Z_A > 0)
//    for (auto it = terms.begin(); it != terms.end(); ++it) {
//        pres_assoc_bin_ptr disj = boost::make_shared<EPDisj>();
//        pres_ptr not_generated = make_not_generated(*it);

//        pres_val_ptr z = safe_lookup(*it, &zs);
//        pres_ptr gtzero = boost::make_shared<EPLessThan>(zero, z);

//        disj->add_clause(gtzero);
//        disj->add_clause(not_generated);
//        clause->add_clause(disj);
//    }

    for (auto it = nonterms.begin(); it != nonterms.end(); ++it) {
        pres_val_ptr z = safe_lookup(*it, &zs);
        make_second_bullet_big_disjunct(*it, zs, zero, one, z, clause);
    }

    // extra sub-bullet for missing clauses 
    // (X_A = 0 or A is init or Z_A > 0)
    for (auto it = nonterms.begin(); it != nonterms.end(); ++it) {
        pres_assoc_bin_ptr disj = boost::make_shared<EPDisj>();
        pres_ptr not_generated = make_not_generated(*it);

        pres_val_ptr in_head = get_nonterm_val(*it);
        pres_ptr is_init = boost::make_shared<EPLessThan>(zero, in_head);

        pres_val_ptr z = safe_lookup(*it, &zs);
        pres_ptr gtzero = boost::make_shared<EPLessThan>(zero, z);

        disj->add_clause(not_generated);
        disj->add_clause(is_init);
        disj->add_clause(gtzero);
        clause->add_clause(disj);
    }

    for (auto it = terms.begin(); it != terms.end(); ++it) {
        pres_val_ptr z = safe_lookup(*it, &zs);
        make_second_bullet_big_disjunct(*it, zs, zero, one, z, clause);
    }


    // quantification
    set<string> z_list;
    for (auto it = zs.begin(); it != zs.end(); ++it) {
        z_list.insert(it->second->get_name());
    }

    pres_ptr fmla = boost::make_shared<EPExists>(z_list, (pres_ptr&)clause);

    return fmla;
}

void CFG2Pres::make_second_bullet_big_disjunct(string const& character,
                                               map<string, pres_var_ptr> const& zs,
                                               pres_val_ptr& zero,
                                               pres_val_ptr& one,
                                               pres_val_ptr& z,
                                               pres_assoc_bin_ptr& clause) {
    // Moved above (X_A = 0 or Z_A > 0) for term
    //             (X_A = 0 or A init or Z_A > 0) for nonterm
    // 
    // the formula in verma et al is roughly
    //
    //     (Z_A = 0) or OR_{B}(Z_A = Z_B + 1 and y_p > 0 and Z_B > 0) OR_{init
    //     B}(Z_A = 1 and y_p > 0)
    //
    // this allows completely disjoint rules A -> a A to be executed as many
    // times as we want by setting Z_A = 0.  So, we actually want
    //
    //     (Z_A = 0 and (A is init or A is not produced)) or ...
    //
    // for a nonterminal A, not for a terminal

    pres_assoc_bin_ptr disj = boost::make_shared<EPDisj>();
    pres_ptr eqone = boost::make_shared<EPEqual>(z, one);

    set<cfg::rule_const_ptr> const& body_rules = cfg->rules_by_body(character);
    for (auto it_rules = body_rules.begin(); it_rules != body_rules.end(); ++it_rules) {
        auto rule = *it_rules;
        auto head = rule->get_head();
        pres_val_ptr z_head = safe_lookup(head, &zs);
        pres_val_ptr in_head = get_nonterm_val(head);
        pres_val_ptr var_rule = get_rule_var(rule);

        // (in_head > 0 && z == 1 && var_rule > 0) 
        pres_assoc_bin_ptr conj = boost::make_shared<EPConj>();
        pres_ptr head_gt_zero = boost::make_shared<EPLessThan>(zero, in_head);
        pres_ptr rule_gt_zero = boost::make_shared<EPLessThan>(zero, var_rule);

        conj->add_clause(head_gt_zero);
        conj->add_clause(eqone);
        conj->add_clause(rule_gt_zero);

        disj->add_clause(conj);

        // z != z + 1 ever...
        if (z != z_head) {
            // or (in_head = 0 && z > 0 && z = z_head + 1 && var_rule > 0 && z_head > 0)
            pres_assoc_bin_ptr conj2 = boost::make_shared<EPConj>();
            pres_plus_ptr z_head_p1 = boost::make_shared<EPPlus>();
            z_head_p1->add_operand(z_head);
            z_head_p1->add_operand(one);
            pres_ptr z_eq_zhead_p1 = boost::make_shared<EPEqual>(z, (pres_val_ptr&)z_head_p1);
            pres_ptr z_head_gt_zero = boost::make_shared<EPLessThan>(zero, z_head);
            pres_ptr in_head_zero = boost::make_shared<EPEqual>(zero, in_head);
            pres_ptr z_gt_zero = boost::make_shared<EPLessThan>(zero, z);

//            conj2->add_clause(in_head_zero);
//            conj2->add_clause(z_gt_zero);
            conj2->add_clause(z_eq_zhead_p1);
            conj2->add_clause(z_head_gt_zero);
            conj2->add_clause(rule_gt_zero);

            disj->add_clause(conj2);
        }
    }

//    pres_ptr eqzero = boost::make_shared<EPEqual>(z, zero);
//    disj->add_clause(eqzero);

    pres_ptr not_generated = make_not_generated(character);
    pres_ptr eqzero = boost::make_shared<EPEqual>(z, zero);

    if (cfg->has_nonterminal(character)) {
        // add (Z_A = 0 and (A is init or A not generated))
        pres_val_ptr in_head = get_nonterm_val(character);
        pres_ptr is_init = boost::make_shared<EPLessThan>(zero, in_head);
        pres_ptr eqzerocond = boost::make_shared<EPDisj>(is_init, (pres_ptr&)not_generated);

        pres_assoc_bin_ptr eqzeroconj = boost::make_shared<EPConj>(eqzerocond, eqzero);

        disj->add_clause(eqzeroconj);
    } else {
        // add (Z_A = 0 and (A not generated))
        pres_assoc_bin_ptr eqzeroconj = boost::make_shared<EPConj>((pres_ptr&)not_generated, eqzero);
        disj->add_clause(eqzeroconj);
    }
 
    clause->add_clause(disj);
}


pres_val_ptr CFG2Pres::get_nonterm_val(string const& nonterm) {
    return safe_lookup(nonterm, nonterm_vals);
}


pres_var_ptr CFG2Pres::get_rule_var(cfg::rule_const_ptr rule) {
    auto result = rules_vars->find(rule);
    if (result == rules_vars->end()) {
        cerr << "CFG2Pres::get_rule_var could not find " 
             << *rule
             << " in map" << endl;
        exit(-1);
    }
    return result->second;
}

template <class T, class U> 
U CFG2Pres::safe_lookup(T const& key, map<T, U> const* map) {
    auto result = map->find(key);
    if (result == map->end()) {
        cerr << "CFG2Pres::safe_lookup could not find " 
             << key
             << " in map" << endl;
        exit(-1);
    }
    return result->second;
}





void CFG2Pres::make_rule_vars(cfg::CFG const& cfg, 
                              string const& prefix,
                              map<cfg::rule_const_ptr, pres_var_ptr>& result) {
    make_map(cfg.get_rules(), prefix, result);
}


void CFG2Pres::make_nonterm_vars(cfg::CFG const& cfg, 
                                 string const& prefix,
                                 map<string, pres_var_ptr>& result) {
    make_map(cfg.get_nonterminals(), prefix, result);
}

void CFG2Pres::make_term_vars(cfg::CFG const& cfg, 
                              string const& prefix,
                              map<string, pres_var_ptr>& result) {
    make_map(cfg.get_terminals(), prefix, result);
}

template <class T, class U> 
void CFG2Pres::make_map(T const& vars, 
                        string const& prefix,
                        map<U, pres_var_ptr>& result) {
    int count = 0;
    for (auto it = vars.begin(); it != vars.end(); ++it) {
        stringstream suffix;
        suffix << count++;
        pres_var_ptr v = boost::make_shared<EPVar>(prefix + suffix.str());
        result.insert(make_pair(*it, v));
    }
}

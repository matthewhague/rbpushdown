
#include <set>
#include <vector>
#include <string>
#include <iostream>

#include "cfg.h"

using namespace std;
using namespace cfg;

int Rule::next_id = 0;

Rule::Rule(string const& new_head, vector<string> const& new_body)
    : head(new_head),
      body(new_body) {
    id = next_id++;      
}


bool Rule::operator<(Rule const& rhs) const {
//    int result = head.compare(rhs.get_head());
//    if (result == 0) {
//        if (body < rhs.get_body()) {
//            result = -1;
//        }
//    }
//    return (result < 0);
    return id < rhs.get_id();
}

bool cfg::operator<(rule_const_ptr lhs, rule_const_ptr rhs) {
    if (!rhs)
        return 0;
    else if (!lhs /*&& rhs*/)
        return 1;
    else 
        return *lhs < *rhs;
}


ostream& cfg::operator<<(ostream& output,  Rule const& r) {
    output << r.head << " -> ";
    for (auto it = r.body.begin(); it != r.body.end(); ++it) {
        output << *it << " ";
    }
    return output;
}


CFG::CFG(vector<rule_const_ptr> const& new_rules,
         set<string> const& new_nonterminals,
         set<string> const& new_terminals) 
    : rules(new_rules),
      nonterminals(new_nonterminals),
      terminals(new_terminals) {
    rebuild_rules_maps();
}

CFG::CFG(vector<rule_const_ptr> const& new_rules) {
    set<string> empty_nonterms;
    remake_with_rules(new_rules, empty_nonterms);
}

void CFG::remake_with_rules(vector<rule_const_ptr> const& new_rules, 
                            set<string> const& old_nonterms) {
    rules = new_rules;

    for(auto it = rules.begin(); it != rules.end(); ++it) {
        nonterminals.insert((*it)->get_head());
    }

    terminals.clear();
    for(auto it = rules.begin(); it != rules.end(); ++it) {
        auto rhs = (*it)->get_body();
        for(auto it_rhs = rhs.begin(); it_rhs != rhs.end(); ++it_rhs) {
            // if it used to be a nonterm, it still is
            if (old_nonterms.find(*it_rhs) != old_nonterms.end()) {
                nonterminals.insert(*it_rhs);
            } else {
                terminals.insert(*it_rhs);
            }
        }
    }

    rebuild_rules_maps();
}


void CFG::add_rule(rule_const_ptr rule) {
    add_nonterminal(rule->get_head());
    rules.push_back(rule);
    add_rule_to_maps(rule);
}


void CFG::add_nonterminal(string const& nonterm) {
    nonterminals.insert(nonterm);
}

void CFG::add_terminal(string const& term) {
    terminals.insert(term);
}


void CFG::rebuild_rules_maps() {
    head_map.clear();
    body_map.clear();
    int pos = 0;
    for (auto it = rules.begin(); it != rules.end(); ++it) {
        add_rule_to_maps(*it);
    }
}


void CFG::add_rule_to_maps(rule_const_ptr rule) {
    add_string_rule_map(rule->get_head(), rule, head_map);
    vector<string> body = rule->get_body();
    for (auto it = body.begin(); it != body.end(); ++it) {
        add_string_rule_map(*it, rule, body_map);
    }
}

void CFG::add_string_rule_map(string const& s, 
                              rule_const_ptr& rule, 
                              map<string, set<rule_const_ptr>>& map) {
    auto mapping = map.find(s);
    if (mapping != map.end()) {
        mapping->second.insert(rule);
    } else {
        set<rule_const_ptr> r_set;
        r_set.insert(rule);
        map.insert(make_pair(s, r_set));
    }
}


set<rule_const_ptr> const& 
    CFG::safe_lookup(string const& elem, 
                     map<string, set<rule_const_ptr>> const& map) const {
    auto result = map.find(elem);
    if (result == map.end()) {
        return empty_rule_vec;
    } else {
        return result->second;
    }
}



ostream& cfg::operator<<(ostream& output, CFG const& g) {
    output << "Non-terminals: ";
    for (auto it = g.nonterminals.begin(); it != g.nonterminals.end(); ++it) {
        output << *it << " ";
    }
    output << "\n\n";

    output << "Terminals: ";
    for (auto it = g.terminals.begin(); it != g.terminals.end(); ++it) {
        output << *it << " ";
    }
    output << "\n\n";

    for (auto it = g.rules.begin(); it != g.rules.end(); ++it) {
        output << *(*it) << "\n";
    }

    return output;
}


int Rule::count_in_body(string const& nt) const {
    int count = 0;
    for (auto it = body.begin(); it != body.end(); ++it) {
        if (*it == nt)
            ++count;
    }
    return count;
}


void CFG::minimise_cfg(set<string> const& initials) {
    vector<rule_const_ptr> new_rules;
    set<string> worklist = initials;
    set<string> done;
    while (!worklist.empty()) {
        auto nonterm = *worklist.begin();
        worklist.erase(nonterm);
        done.insert(nonterm);
        auto rules = rules_by_head(nonterm);
        for (auto it = rules.begin(); it != rules.end(); ++it) {
            new_rules.push_back(*it);
            auto body = (*it)->get_body();
            for (auto lhs_it = body.begin(); lhs_it != body.end(); ++lhs_it) {
                auto is_nonterm = nonterminals.find(*lhs_it);
                if (is_nonterm != nonterminals.end()) {
                    auto is_done = done.find(*lhs_it);
                    if (is_done == done.end()) {
                        worklist.insert(*lhs_it);
                    }
                }
            }
        }
    }

    set<string> old_nonterms = nonterminals;

    remake_with_rules(new_rules, old_nonterms);
}



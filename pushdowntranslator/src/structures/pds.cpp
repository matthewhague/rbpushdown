
#include <iostream>
#include <vector>
#include <set>
#include <string>


#include "boost/make_shared.hpp"

#include "../tools/tools.h"

#include "pds.h"


using namespace std;
using namespace pds;



int Rule::next_id = 0;

Rule::Rule(string const& new_p, 
           string const& new_a,
           vector<string> const& new_actions,
           string const& new_q, 
           vector<string> const& new_w,
           cexp::counterexp_ptr const& new_guard,
           set<cact> const& new_counter_acts)
    : p(new_p),
      a(new_a),
      actions(new_actions),
      q(new_q),
      w(new_w),
      guard(new_guard),
      counter_acts(new_counter_acts) {
    id = next_id++;
}


Rule::Rule(string const& new_p, 
           string const& new_a,
           string const& new_action,
           string const& new_q, 
           vector<string> const& new_w,
           cexp::counterexp_ptr const& new_guard,
           set<cact> const& new_counter_acts)
    : p(new_p),
      a(new_a),
      q(new_q),
      w(new_w),
      guard(new_guard),
      counter_acts(new_counter_acts) {
    id = next_id++;
    actions.push_back(new_action);
}

Rule::~Rule() {
}


bool pds::operator<(rule_const_ptr lhs, rule_const_ptr rhs) {
    if (!rhs)
        return 0;
    else if (!lhs /*&& rhs*/)
        return 1;
    else {
        int acmp = lhs->get_a().compare(rhs->get_a());
        if (acmp != 0)
            return (acmp < 0);
        else {
            int pcmp = lhs->get_p().compare(rhs->get_p());
            if (pcmp != 0) 
                return (pcmp < 0);
            else
                return lhs->get_id() < rhs->get_id();
        }
    }
}



ostream& pds::operator<<(ostream& output, Rule const& r) {
    output << r.p << " " << r.a 
           << " -- "; 
    tools::write_list(r.actions, output, [&](string const& action) {
        output << action;
    });
    output << " --> " << r.q << " ";
    for (auto it = r.w.begin(); it != r.w.end(); ++it) {
        output << *it << " ";        
    }

    if (r.guard) {
        output << "[" << *(r.guard) << "] ";
    }

    for (cact const& cact : r.counter_acts) {
        output << cact.first;
        if (cact.second < 0)
            output << " -= ";
        else
            output << " += ";
        output << cact.second << " ";
    }

    output << ";";

    return output;
}

Pds::Pds() : rules() {
}

Pds::Pds(set<rule_const_ptr> const& new_rules) {
    for (auto it = rules.begin(); it != rules.end(); ++it) {
        add_rule(*it);
    }
}

Pds::~Pds() {
}

bool Pds::add_rule(rule_const_ptr r) {
    bool add = !has_rule_match(r);
    if (add) {
        rules.insert(r);
        update_info(r);
        add_rule_to_maps(r);
    }
    return add;
}

void Pds::del_rule(rule_const_ptr r) {
    rules.erase(r);
    auto head = make_head(r->get_p(), r->get_a());
    del_rule_from_map(rule_lookup, head, r);
    vector<string> const& w = r->get_w();
    if (w.size() > 0) {
        head = make_head(r->get_q(), w[0]);
        del_rule_from_map(rule_rev_lookup, head, r);
    }
}


void Pds::del_rule_from_map(std::map<head, std::set<rule_const_ptr>>& m,
                            head h,
                            rule_const_ptr r) {
    auto map_set = m.find(h);
    if (map_set != m.end()) {
        map_set->second.erase(r);
    }
}


void Pds::del_rule_from_maps(rule_const_ptr r) {
    auto head = make_head(r->get_p(), r->get_a());
    del_rule_from_map(rule_lookup, head, r);

    vector<string> const& w = r->get_w();
    if (w.size() > 0) {
        head = make_head(r->get_q(), w[0]);
        del_rule_from_map(rule_rev_lookup, head, r);
    }
}


void Pds::add_rules(std::set<rule_const_ptr> const& add_rules) {
    for (rule_const_ptr r : add_rules) {
        add_rule(r);
    }
}

void Pds::del_rules(std::set<rule_const_ptr> const& del_rules) {
    for (rule_const_ptr r : del_rules) {
        del_rule(r);
    }
}


bool Pds::has_rule_match(rule_const_ptr r) {
    bool res = 0;
    auto it = rule_lookup.find(make_head(r->get_p(), r->get_a()));
    if (it != rule_lookup.end()) {
        auto r_it = it->second.begin(); 
        while (!res && (r_it != it->second.end())) {
            res = rule_match(r, *r_it++);
        }
    }
    return res;
}

bool Pds::rule_match(rule_const_ptr r1, rule_const_ptr r2) {
    bool res = 0;
    // we don't match rules at the moment
    if (!r1->get_guard() && !r2->get_guard()) {
        res = (r1->get_p() == r2->get_p()) &&
              (r1->get_a() == r2->get_a()) &&
              (r1->get_actions() == r2->get_actions()) &&
              (r1->get_q() == r2->get_q()) &&
              (r1->get_w() == r2->get_w()) &&
              (r1->get_counter_acts() == r2->get_counter_acts());
    }
    return res;
}


void Pds::update_info(rule_const_ptr r) {
    controls.insert(r->get_p());
    controls.insert(r->get_q());
    alphabet.insert(r->get_a());
    auto w = r->get_w();
    for (auto it = w.begin(); it != w.end(); ++it) {
        alphabet.insert(*it);
    }
    for (string const& action : r->get_actions()) {
        actions.insert(action);
    }
    for (cact const& cact : r->get_counter_acts()) {
        counters.insert(cact.first);
    }
}

void Pds::rebuild_info() {
    controls.clear();
    alphabet.clear();
    actions.clear();
    counters.clear();
    for (auto it = rules.begin(); it != rules.end(); ++it) {
        update_info(*it);
    }
}


void Pds::add_rule_to_map(map<head, set<rule_const_ptr>>& m,
                          head h,
                          rule_const_ptr r) {
    auto mapping = m.find(h);
    if (mapping != m.end()) {
        mapping->second.insert(r);
    } else {
        set<rule_const_ptr> r_set;
        r_set.insert(r);
        m.insert(make_pair(h, r_set));
    }
}

void Pds::add_rule_to_maps(rule_const_ptr r) {
    auto head = make_head(r->get_p(), r->get_a());
    add_rule_to_map(rule_lookup, head, r);

    vector<string> const& w = r->get_w();
    if (w.size() > 0) {
        head = make_head(r->get_q(), w[0]);
        add_rule_to_map(rule_rev_lookup, head, r);
    }
}

ostream& pds::operator<<(ostream& output, Pds const& pds) {
    output << "Controls: ";
    for (auto it = pds.controls.begin(); it != pds.controls.end(); ++it) {
        output << *it << " ";
    }
    output << "\n\n";
 
    output << "Alphabet: ";
    for (auto it = pds.alphabet.begin(); it != pds.alphabet.end(); ++it) {
        output << *it << " ";
    }
    output << "\n\n";

    output << "Actions: ";
    for (auto it = pds.actions.begin(); it != pds.actions.end(); ++it) {
        output << *it << " ";
    }
    output << "\n\n";
        
    output << "Counters: ";
    for (auto it = pds.counters.begin(); it != pds.counters.end(); ++it) {
        output << *it << " ";
    }
    output << "\n\n";

    if (pds.init_p != "") {
        cout << "Init Control: " << pds.init_p << "\n";
    }
    if (pds.init_a != "") {
        cout << "Init Stack: " << pds.init_a << "\n";
    }
    if (pds.fin_p != "") {
        cout << "Final Control: " << pds.fin_p << "\n";
    }

    output << "Rules: \n\n";
    for (auto it = pds.rules.begin(); it != pds.rules.end(); ++it) {
        output << *(*it) << "\n";
    }
    output << "\n";

    return output;
}


void Pds::remove_unreachable() {
    if (get_init_p() != "" && get_init_a() != "") {
        remove_unreachable(get_init_p(), get_init_a());
    }
}

void Pds::remove_unreachable(string const& init_p, string const& init_a) {
    set<pair<string, string>> reach_heads;
    get_reachable_heads(init_p, init_a, reach_heads);
    set<rule_const_ptr> to_remove;

    for (auto it = rules.begin(); it != rules.end(); ++it) {
        auto rule = *it;
        auto head = make_head(rule->get_p(), rule->get_a());
        if (reach_heads.find(head) == reach_heads.end()) {
            to_remove.insert(rule);
        }
    }

    for (auto it = to_remove.begin(); it != to_remove.end(); ++it) {
        del_rule(*it);
    }
}


void Pds::get_reachable_heads(string const& init_p, 
                              string const& init_a, 
                              set<pair<string, string>>& reach_heads) {
    set<pair<string, string>> new_heads;
    auto init_head = make_head(init_p, init_a);
    new_heads.insert(init_head);
    reach_heads.insert(init_head);

    while (new_heads.size() > 0) {
        auto head = *new_heads.begin();
        new_heads.erase(head);
        auto rules_pair = rule_lookup.find(head);
        if (rules_pair != rule_lookup.end()) {
            auto rules = rules_pair->second;
            for (auto it = rules.begin(); it != rules.end(); ++it) {
                auto q = (*it)->get_q();
                auto w = (*it)->get_w();
                if (w.size() >= 1) {
                    auto new_head = make_head(q, w.front());
                    if (reach_heads.find(new_head) == reach_heads.end()) {
                        reach_heads.insert(new_head);
                        new_heads.insert(new_head);
                    }
                }
                if (w.size() > 1) {
                    for (auto chars = ++w.begin(); chars != w.end(); ++chars) {
                        for (auto qs = controls.begin(); qs != controls.end(); ++qs) {
                            auto new_head = make_head(*qs, *chars);
                            if (reach_heads.find(new_head) == reach_heads.end()) {
                                reach_heads.insert(new_head);
                                new_heads.insert(new_head);
                            }
                        }
                    }
                }
            }
        }
    }
}


std::set<rule_const_ptr> const& Pds::get_rules(string const& p,
                                               string const& a) const {
    auto it = rule_lookup.find(make_head(p, a));
    if (it != rule_lookup.end()) {
        return it->second;
    } 

    return empty_rule_set;
}

std::set<rule_const_ptr> const& Pds::get_rev_rules(string const& p,
                                                   string const& a) const {
    auto it = rule_rev_lookup.find(make_head(p, a));
    if (it != rule_rev_lookup.end()) {
        return it->second;
    } 

    return empty_rule_set;
}


int Pds::get_reversals(std::string const& counter) const {
    auto it = counter_revs.find(counter);
    if (it == counter_revs.end()) {
        cerr << "Program::get_reversals has no reversals setting for counter " 
             << counter 
             << endl;
        exit(-1);
    }
    return it->second;
}

void Pds::set_reversals(std::string const& counter, int reversals) {
    auto it = counter_revs.find(counter);
    if (it == counter_revs.end()) {
        counter_revs.insert(make_pair(counter, reversals));
    } else {
        it->second = reversals;
    }
}



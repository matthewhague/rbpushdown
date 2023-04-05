
#include <iostream>
#include <vector>
#include <set>
#include <string>

#include "pds.h"

#include "boost/make_shared.hpp"

using namespace std;
using namespace pds;



int Rule::next_id = 0;

Rule::Rule(string const& new_p, 
           string const& new_a,
           string const& new_action,
           string const& new_q, 
           vector<string> const& new_w,
           rule_guard_ptr const& new_guard,
           set<string> const& new_incs,
           set<string> const& new_decs)
    : p(new_p),
      a(new_a),
      action(new_action),
      q(new_q),
      w(new_w),
      guard(new_guard),
      incs(new_incs),
      decs(new_decs) {
    id = next_id++;
}

Rule::~Rule() {
}

//bool Rule::operator<(Rule const& rhs) const {
//    int result = p.compare(rhs.get_p());
//    if (result == 0) {
//        result = a.compare(rhs.get_a());
//        if (result == 0) {
//            result = action.compare(rhs.get_action());
//            if (result == 0) {
//                result = q.compare(rhs.get_q());
//                if (result == 0) {
//                    if (w < rhs.get_w()) {
//                        result = -1;
//                    } else if (!(w > rhs.get_w())) {
//                        // now compare rules -- this is too much for very
//                        // little, just use a vector of rules for now...
//                    }
//                }
//            }
//        }
//    }
//    return (result < 0);
//}



bool pds::operator<(rule_const_ptr lhs, rule_const_ptr rhs) {
    if (!rhs)
        return 0;
    else if (!lhs /*&& rhs*/)
        return 1;
    else 
        return lhs->get_id() < rhs->get_id();
}



ostream& pds::operator<<(ostream& output, Rule const& r) {
    output << r.p << " " << r.a 
           << " -- " << r.action << " --> " << r.q << " ";
    for (auto it = r.w.begin(); it != r.w.end(); ++it) {
        output << *it << " ";        
    }

    if (r.guard) {
        output << "[" << *(r.guard) << "] ";
    }

    for (auto it = r.incs.begin(); it != r.incs.end(); ++it) {
        output << "+" << *it << " ";
    }

    for (auto it = r.decs.begin(); it != r.decs.end(); ++it) {
        output << "-" << *it << " ";
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
        update_maps(r);
    }
    return add;
}

bool Pds::has_rule_match(rule_const_ptr r) {
    bool res = 0;
    auto it = rule_lookup.find(make_pair(r->get_p(), r->get_a()));
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
              (r1->get_action() == r2->get_action()) &&
              (r1->get_q() == r2->get_q()) &&
              (r1->get_w() == r2->get_w()) &&
              (r1->get_incs() == r2->get_incs()) &&
              (r1->get_decs() == r2->get_decs());
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
    actions.insert(r->get_action());
    auto incs = r->get_incs();
    for (auto it = incs.begin(); it != incs.end(); ++it) {
        counters.insert(*it);
    }
    auto decs = r->get_decs();
    for (auto it = decs.begin(); it != decs.end(); ++it) {
        counters.insert(*it);
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

void Pds::update_maps(rule_const_ptr r) {
    auto head = make_pair(r->get_p(), r->get_a());
    auto mapping = rule_lookup.find(head);
    if (mapping != rule_lookup.end()) {
        mapping->second.insert(r);
    } else {
        set<rule_const_ptr> r_set;
        r_set.insert(r);
        rule_lookup.insert(make_pair(head, r_set));
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

    output << "Rules: \n\n";
    for (auto it = pds.rules.begin(); it != pds.rules.end(); ++it) {
        output << *(*it) << "\n";
    }
    output << "\n";

//    output << "Rules: \n\n";
//    for (auto it = pds.rule_lookup.begin(); it != pds.rule_lookup.end(); ++it) {
//        auto head = it->first;
//        auto vec = it->second;
//        output << "    " <<  head.first << " " << head.second << ":\n";
//        for (auto it_rules = vec.begin(); it_rules != vec.end(); it_rules++) {
//            output << "        " << *(*it_rules) << "\n";
//        }
//    }

    return output;
}



void RGAssocBin::to_stream(ostream& output) const {
    output << "(";
    auto it = clauses.begin();
    while (it != clauses.end()) {
        output << (*it);
        if (++it != clauses.end()) 
            output << " " << get_connective() << " ";
    }
    output << ")";
}

void Pds::del_rule(rule_const_ptr r) {
    rules.erase(r);
    auto head = make_pair(r->get_p(), r->get_a());
    auto map_set = rule_lookup.find(head);
    if (map_set != rule_lookup.end()) {
        map_set->second.erase(r);
    }
}

// the idea is to compress all p1 a1 --> p2 a2; p2 a2 --> p3 a3 to p1 a1 --> p3 a3
void Pds::minimise(string const& init_p, string const& init_a) {
    compress_rules();
    remove_unreachable(init_p, init_a);
    rebuild_info();
}


void Pds::get_unsilent_heads(set<pair<string, string>>& unsilent_heads) {
    for (auto it = rules.begin(); it != rules.end(); ++it) {
        auto r = *it;
        if (   r->get_guard()
            || r->get_incs().size() > 0
            || r->get_decs().size() > 0
            || r->get_w().size() > 1
            || r->get_w().size() == 0
            || r->get_action() != E_ACT) {
            unsilent_heads.insert(make_pair(r->get_p(), r->get_a()));
        }
    }
}


// rules going into head, -1 if it's part of a return
void Pds::get_counts_in(map<pair<string, string>, int>& counts_in) {
    for (auto it = rules.begin(); it != rules.end(); ++it) {
        auto r = *it;
        auto w = r->get_w();
        if (w.size() == 1) {
            auto head = make_pair(r->get_q(), w.front());
            auto mapping = counts_in.find(head);
            if (mapping != counts_in.end()) {
                if (mapping->second > -1)
                    ++mapping->second;
            } else {
                counts_in.insert(make_pair(head, 1));
            }
        } else if (w.size() > 1){
            for (auto ps = controls.begin(); ps != controls.end(); ++ps) {
                auto head = make_pair(r->get_q(), w.front());
                auto mapping = counts_in.find(head);
                if (mapping != counts_in.end()) {
                    mapping->second = -1;
                } else {
                    counts_in.insert(make_pair(head, -1));
                }
            }
        }
    }
}


void Pds::compress_rules() {
    set<pair<string, string>> unsilent_heads;
    get_unsilent_heads(unsilent_heads);
    map<pair<string, string>, int> counts_in;
    get_counts_in(counts_in);
    
    bool change = 1;
    set<rule_const_ptr> to_remove;
    set<rule_const_ptr> to_add;
    while (change) {
        to_remove.clear();
        to_add.clear();
        change = 0;
        for (auto it = rules.begin(); 
             it != rules.end() && to_remove.size() == 0 && to_add.size() == 0; 
             ++it) {
            auto rule = *it;
            if (rule->get_w().size() == 1 && (to_remove.find(rule) == to_remove.end())) {
                auto rule_head = make_pair(rule->get_p(), rule->get_a());
                auto body_head = make_pair(rule->get_q(), rule->get_w().front());


                // if both silent and count_in next = 1 and count_out next <= 1
                auto cin_map = counts_in.find(body_head);
                bool counts_in_1 = 0;
                if (cin_map != counts_in.end())
                    counts_in_1 = (cin_map->second == 1);
                bool counts_out_1 = 1;
                auto next_r_pair = rule_lookup.find(body_head);
                if (next_r_pair != rule_lookup.end()) 
                    counts_out_1 = (next_r_pair->second.size() <= 1);


                if (counts_in_1 && counts_out_1 &&
                    unsilent_heads.find(rule_head) == unsilent_heads.end() && 
                    unsilent_heads.find(body_head) == unsilent_heads.end()) {
                    if (next_r_pair != rule_lookup.end()) {
                        // is only 1 rule
                        auto next_rules = next_r_pair->second;
                        auto next_rule = *next_rules.begin();
                        auto p = rule->get_p();
                        auto a = rule->get_a();
                        auto q = next_rule->get_q();
                        auto w = next_rule->get_w();
                        rule_guard_ptr g;
                        set<string> incs;
                        set<string> decs;
                        if (p != q || a != w.front()) {
                            to_add.insert(boost::make_shared<Rule>(p, a, E_ACT, q, w, g, incs, decs));
                        }
                        to_remove.insert(rule);
                        to_remove.insert(next_rule);

                        // should really set counts_in(next_rule_head) = 0
                        // but since it will never be referenced again, let's
                        // not bother...
                    }
                }
            }
        }

        for (auto it = to_remove.begin(); it != to_remove.end(); ++it) {
            bool has_match = 0;
            auto it_add = to_add.begin(); 
            while (!has_match && it_add != to_add.end()) {
                has_match = rule_match(*it, *it_add);
                ++it_add;
            }
            if (!has_match) {
                change = 1;
                del_rule(*it);
            }
        }

        for (auto it = to_add.begin(); it != to_add.end(); ++it) {
            change |= add_rule(*it);
        }
    }
}


void Pds::remove_unreachable(string const& init_p, string const& init_a) {
    set<pair<string, string>> reach_heads;
    get_reachable_heads(init_p, init_a, reach_heads);
    set<rule_const_ptr> to_remove;

    for (auto it = rules.begin(); it != rules.end(); ++it) {
        auto rule = *it;
        auto head = make_pair(rule->get_p(), rule->get_a());
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
    auto init_head = make_pair(init_p, init_a);
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
                    auto new_head = make_pair(q, w.front());
                    if (reach_heads.find(new_head) == reach_heads.end()) {
                        reach_heads.insert(new_head);
                        new_heads.insert(new_head);
                    }
                }
                if (w.size() > 1) {
                    for (auto chars = ++w.begin(); chars != w.end(); ++chars) {
                        for (auto qs = controls.begin(); qs != controls.end(); ++qs) {
                            auto new_head = make_pair(*qs, *chars);
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




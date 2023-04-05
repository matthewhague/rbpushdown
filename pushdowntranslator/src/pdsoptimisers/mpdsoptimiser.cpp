

#include <set>

#include "boost/make_shared.hpp"

#include "mpdsoptimiser.h"

using namespace std;
using namespace pds;

void MPDSOptimiser::get_switch_controls(set<string>& switch_controls, 
                                        multipds_ptr mpds) {
    for (globalrule_ptr grule : mpds->get_global_rules()) {
        for (string const& control : grule->get_controls_before()) {
            switch_controls.insert(control);
        }
    }
}


void MPDSOptimiser::get_removable_stacks(set<string>& removable_stacks,
                                         set<string> const& switch_controls,
                                         pds_ptr pds,
                                         multipds_ptr mpds) {
    set<string> unremovable_stacks;
    get_unremovable_stacks(unremovable_stacks,
                           switch_controls,
                           pds,
                           mpds);

    removable_stacks.clear();
    for (string const& stack : pds->get_alphabet()) {
        if (!unremovable_stacks.count(stack)) {
            removable_stacks.insert(stack);
        }
    }
}

void MPDSOptimiser::get_unremovable_stacks(set<string>& unremovable_stacks, 
                                           set<string> const& switch_controls,
                                           pds_ptr pds,
                                           multipds_ptr mpds) {
    // First, any that appear as return locations -- don't remove rules that
    // might kill a return location.
    for (rule_const_ptr rule : pds->get_rules()) {
        vector<string> const& w = rule->get_w();
        if (w.size() > 1) {
            auto it = ++(w.begin());
            auto itend = w.end();
            while (it != itend) {
                unremovable_stacks.insert(*it);
                ++it;
            }
        }
    }

    // Second, any that appear with a switchable control
    // The final state isn't a problem if there's no way of getting out of it
    // (this is true of all states, but let's not bother checking it)
    bool final_is_sink = check_final_is_sink(pds, mpds);
    string const& final = pds->get_fin_p();
    for (rule_const_ptr rule : pds->get_rules()) {
        if ((!final_is_sink || rule->get_p() != final) &&
            switch_controls.count(rule->get_p())) {
            unremovable_stacks.insert(rule->get_a());
        }
        vector<string> const& w = rule->get_w();
        if (w.size() > 0 && 
            (!final_is_sink || rule->get_q() != final) &&
            switch_controls.count(rule->get_q())) {
            unremovable_stacks.insert(w[0]);
        }
    }

    // Third, any that have a counter test or a counter update, or occur
    // immediately after a counter update
    for (rule_const_ptr rule : pds->get_rules()) {
        if (rule->get_guard()) {
            unremovable_stacks.insert(rule->get_a());
        }
        if (rule->get_counter_acts().size() > 0) {
            unremovable_stacks.insert(rule->get_a());
            vector<string> const& w = rule->get_w();
            if (w.size() > 0) {
                unremovable_stacks.insert(w[0]);
            }
            // note: the above misses pop rules, but all return locations (of a
            // pop rule) are considered unremovable_stacks anyhow.
        }
    }
}

bool MPDSOptimiser::check_final_is_sink(pds_ptr pds, multipds_ptr mpds) {
    string const& fin = pds->get_fin_p();
    bool final_is_sink = true;
    auto it = pds->get_rules().begin();
    auto itend = pds->get_rules().end();
    while (final_is_sink && it != itend) {
        if ((*it)->get_p() == fin) {
            final_is_sink = ((*it)->get_q() == fin);
        }
        ++it;
    }
    auto git = mpds->get_global_rules().begin();
    auto gitend = mpds->get_global_rules().end();
    while (final_is_sink && git != gitend) {
        vector<string> const& before = (*git)->get_controls_before();
        vector<string> const& after  = (*git)->get_controls_after();
        for (unsigned int i = 0; i < before.size(); i++) {
            final_is_sink = before[i] != fin || after[i] == fin;
        }
        ++git;
    }

    return final_is_sink;
}


rule_const_ptr MPDSOptimiser::combine_rules(rule_const_ptr r1, rule_const_ptr r2) {
    rule_const_ptr new_rule;

    vector<string> new_w;
    combine_ws(r1->get_w(), r2->get_w(), new_w);

    cexp::counterexp_ptr new_cc = combine_guards(r1->get_guard(),
                                                 r2->get_guard());
    set<cact> new_cacts; 
    combine_counter_acts(r1->get_counter_acts(), 
                         r2->get_counter_acts(),
                         new_cacts);

    vector<string> new_actions;
    combine_actions(r1->get_actions(), r2->get_actions(), new_actions);

    new_rule = boost::make_shared<Rule>(r1->get_p(),
                                        r1->get_a(),
                                        new_actions,
                                        r2->get_q(),
                                        new_w,
                                        new_cc,
                                        new_cacts);

    return new_rule;
}


void MPDSOptimiser::combine_ws(vector<string> const& w1, 
                               vector<string> const& w2,
                               vector<string>& new_w) {
    // w1 = aw', 
    // new_w = w2 w'
    // because we had pa->qbw', qb->rw2
    for (const string& a : w2) {
        new_w.push_back(a);
    }
    if (w1.size() > 1) {
        auto it = ++(w1.begin());
        auto itend = w1.end();
        while (it != itend) {
            new_w.push_back(*(it++));
        }
    }
}
                 


cexp::counterexp_ptr MPDSOptimiser::combine_guards(cexp::counterexp_ptr cc1,
                                                   cexp::counterexp_ptr cc2) {
    cexp::counterexp_ptr new_cc;

    if (cc1 && cc2) 
        new_cc = boost::make_shared<cexp::CExpAnd>(cc1, cc2);
    else if (cc1 && !cc2) 
        new_cc = cc1;
    else if (!cc1 && cc2) 
        new_cc = cc2;
    else 
        new_cc = cc1;

    return new_cc;
}



void MPDSOptimiser::combine_counter_acts(set<cact> const& cacts1,
                                         set<cact> const& cacts2,
                                         set<cact>& new_cacts) {
    for (cact cact1 : cacts1) {
        new_cacts.insert(cact1);
    }

    for (cact cact2 : cacts2) {
        auto it = new_cacts.begin();
        auto itend = new_cacts.end();
        while (it != itend && it->first != cact2.first) {
            ++it;
        }

        if (it != itend) {
            int new_val = cact2.second + it->second;
            new_cacts.erase(it);
            new_cacts.insert(make_pair(cact2.first, new_val));
        } else {
            new_cacts.insert(cact2);
        }
    }
}


void MPDSOptimiser::combine_actions(vector<string> const& acts1, 
                                    vector<string> const& acts2,
                                    vector<string>& new_acts) {
    for (string const& act : acts1) {
        new_acts.push_back(act);
    }
    for (string const& act : acts2) {
        new_acts.push_back(act);
    }
}


                                  

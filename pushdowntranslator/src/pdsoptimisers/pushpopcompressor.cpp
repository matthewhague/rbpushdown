

#include <set>

#include "boost/make_shared.hpp"

#include "pushpopcompressor.h"

using namespace std;
using namespace pds;

bool PushPopCompressor::optimise(multipds_ptr mpds) {
    bool has_changed = false;
    set<string> switch_controls;
    set<string> removable_stacks; 

    get_switch_controls(switch_controls, mpds);

    for (pds_ptr pds : mpds->get_pdss()) {
            get_removable_stacks(removable_stacks, 
                                 switch_controls, 
                                 pds,
                                 mpds);
//          cout << "had " << pds->get_rules().size() << endl;
            has_changed |= do_remove_calls(pds, removable_stacks);
//          cout << "had " << pds->get_rules().size() << endl;
    }

    return has_changed;
}


bool PushPopCompressor::do_remove_calls(pds_ptr pds, 
                                     std::set<std::string> const& removable_stacks) {
    bool has_changed = false;
    set<rule_const_ptr> new_rules;
    set<rule_const_ptr> del_rules;

    for (rule_const_ptr rule : pds->get_rules()) {
        vector<string> const& w = rule->get_w();
        if (w.size() > 1 && removable_stacks.count(w[0])) {
            string const& q = rule->get_q();
            string const& b = w[0];
            if (only_pop_from(q, b, pds)) {
                for (rule_const_ptr rule2 : pds->get_rules(q, b)) {
                    rule_const_ptr new_rule = combine_rules(rule, rule2);
                    new_rules.insert(new_rule);
                    del_rules.insert(rule2);
                    has_changed = true;

//                    cout << "replacing " << *rule << " and " << *rule2 
//                         << " with " << *new_rule << endl;
                }
                del_rules.insert(rule);
            }
        }
    }

    pds->add_rules(new_rules);
    pds->del_rules(del_rules);

    return has_changed;
}                                  


bool PushPopCompressor::only_pop_from(string const& p, 
                                   string const& a,
                                   pds_ptr pds) {
    bool only_pop = true;
    set<rule_const_ptr> const& from_rules = pds->get_rules(p, a);

    auto it = from_rules.begin();
    auto itend = from_rules.end();
    while (only_pop && it != itend) {
        only_pop = ((*it)->get_w().size() == 0);
        ++it;
    }

    return only_pop;
}                                   

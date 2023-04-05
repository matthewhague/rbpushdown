
#include <set>

#include "boost/make_shared.hpp"

#include "rulecompressor.h"

using namespace std;
using namespace pds;

bool RuleCompressor::optimise(multipds_ptr mpds) {
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
            has_changed |= compress_pds(pds, removable_stacks);
//          cout << "had " << pds->get_rules().size() << endl;
    }

    return has_changed;
}


bool RuleCompressor::compress_pds(pds_ptr pds, 
                                  set<string> const& removable_stacks) {
    bool has_changed = false;
    for (string const& b: removable_stacks) {
        for (string const& q : pds->get_controls()) {
            has_changed |= do_remove_head(q, b, pds);
        }
    }
    return has_changed;
}


bool RuleCompressor::do_remove_head(string const& q, 
                                    string const& b,
                                    pds_ptr pds) {
    bool has_changed = false;
    set<rule_const_ptr> from_rules = pds->get_rules(q, b);
    set<rule_const_ptr> to_rules = pds->get_rev_rules(q, b);

    // First, check that we don't hit a loop
    // (p a --> q b, q b --> q b being replaced by p a --> q b would remove the
    // possibility of looping q b --> q b)
    bool loop = has_loop_from(q, b, pds);

    // Also, don't interrupt any chars that appear on the rhs of a push rule --
    // they are still removable, but we want to avoid building push rules
    // with long rhss.
    bool push = has_push_to(q, b, pds);

    if (!loop && 
        !push &&
        from_rules.size() > 0 && 
        to_rules.size() > 0) {
        set<rule_const_ptr> new_rules;
        for (rule_const_ptr rule_in : to_rules) {
            for (rule_const_ptr rule_out : from_rules) {
                rule_const_ptr new_rule = combine_rules(rule_in, rule_out);
                new_rules.insert(new_rule);
                has_changed = true;

//                cout << "Replacing " << *rule_in << " and " << *rule_out << "\n"
//                     << "With rule " << *new_rule << endl;
            }
        }
        pds->del_rules(to_rules);
        pds->del_rules(from_rules);
        pds->add_rules(new_rules);
    }

    return has_changed;
}

bool RuleCompressor::has_loop_from(string const& q, 
                                   string const& b,
                                   pds_ptr pds) {
    set<rule_const_ptr> from_rules = pds->get_rules(q, b);

    auto it = from_rules.begin();
    auto itend = from_rules.end();
    bool loop = false;
    while (!loop && it != itend) {
        vector<string> const& w = (*it)->get_w();
        if (w.size() > 0) {
            loop = (w[0] == b);
        }
        ++it;
    }

    return loop; 
}

bool RuleCompressor::has_push_to(std::string const& q, 
                                 std::string const& b,
                                 pds_ptr pds) {
    set<rule_const_ptr> to_rules = pds->get_rev_rules(q, b);

    auto it = to_rules.begin();
    auto itend = to_rules.end();
    bool push = false;
    while (!push && it != itend) {
        vector<string> const& w = (*it)->get_w();
        push = (w.size() > 1);
        ++it;
    }

    return push;
}




                                  

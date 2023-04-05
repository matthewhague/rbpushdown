


#include <set>

#include "boost/make_shared.hpp"

#include "silentloopremover.h"

using namespace std;
using namespace pds;

bool SilentLoopRemover::optimise(multipds_ptr mpds) {
    bool has_changed = false;

    for (pds_ptr pds : mpds->get_pdss()) {
//          cout << "had " << pds->get_rules().size() << endl;
            has_changed |= do_remove_loops(pds);
//          cout << "had " << pds->get_rules().size() << endl;
    }

    return has_changed;
}


bool SilentLoopRemover::do_remove_loops(pds_ptr pds) {
    bool has_changed = false;
    set<rule_const_ptr> del_rules;

    for (rule_const_ptr rule : pds->get_rules()) {
        vector<string> const& w = rule->get_w();
        if (w.size() == 1 &&
            rule->get_counter_acts().size() == 0 &&
            !has_non_empty_action(rule) &&
            rule->get_p() == rule->get_q() && 
            rule->get_a() == w[0]) {

            del_rules.insert(rule);
            has_changed = true;
        }
    }

    pds->del_rules(del_rules);

    return has_changed;
}                                  



bool SilentLoopRemover::has_non_empty_action(rule_const_ptr rule) {
    bool yes = false;
    auto it = rule->get_actions().begin();
    auto itend = rule->get_actions().end();
    while (!yes && it != itend) {
        yes = (*it != E_ACT);
        ++it;
    }
    return yes;
}

                                   

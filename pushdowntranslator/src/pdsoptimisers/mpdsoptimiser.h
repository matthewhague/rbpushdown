
#pragma once
#ifndef _PDSOPTIMISER_H_
#define __PDSOPTIMISER_H__

#include "boost/shared_ptr.hpp"

#include "../structures/multi_pds.h"

namespace pds {

    class MPDSOptimiser {
        public:
            // return true if has changed
            virtual bool optimise(multipds_ptr pds) = 0;

        protected:
            void get_switch_controls(std::set<std::string>& switch_controls, 
                                     multipds_ptr mpds);
            void get_removable_stacks(std::set<std::string>& removable_stacks,
                                      std::set<std::string> const& switch_controls,
                                      pds_ptr pds,
                                      multipds_ptr mpds);
            void get_unremovable_stacks(std::set<std::string>& unremovable_stacks, 
                                        std::set<std::string> const& switch_controls,
                                        pds_ptr pds,
                                        multipds_ptr mpds);

            rule_const_ptr combine_rules(rule_const_ptr r1, rule_const_ptr r2);
            cexp::counterexp_ptr combine_guards(cexp::counterexp_ptr cc1,
                                                cexp::counterexp_ptr cc2);
            void combine_counter_acts(std::set<cact> const& cacts1,
                                      std::set<cact> const& cacts2,
                                      std::set<cact>& new_cacts);
            void combine_actions(std::vector<std::string> const& acts1, 
                                 std::vector<std::string> const& acts2,
                                 std::vector<std::string>& new_acts);
            void combine_ws(std::vector<std::string> const& w1, 
                            std::vector<std::string> const& w2,
                            std::vector<std::string>& new_w);
            bool check_final_is_sink(pds_ptr pds, multipds_ptr mpds);
    };

    typedef boost::shared_ptr<MPDSOptimiser> mpdsoptimiser_ptr;

    class NoMPDSOptimiser : public MPDSOptimiser {
        public:
            virtual bool optimise(multipds_ptr prog) { 
                // do nothing 
                return false;
            }
    };

}

#endif

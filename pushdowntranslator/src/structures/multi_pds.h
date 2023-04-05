
#pragma once
#ifndef __MULTI_PDS_H__
#define __MULTI_PDS_H__

#include <vector>

#include "boost/shared_ptr.hpp"

#include "epresburger.h"
#include "pds.h"

namespace pds {

    class GlobalRule;
    class MultiPDS;

    typedef boost::shared_ptr<MultiPDS>   multipds_ptr;
    typedef boost::shared_ptr<GlobalRule> globalrule_ptr;


    class GlobalRule {
        std::vector<std::string> controls_before;
        std::vector<std::string> controls_after;

        public:
            GlobalRule(std::vector<std::string> new_controls_before,
                       std::vector<std::string> new_controls_after)
                : controls_before(new_controls_before),
                  controls_after(new_controls_after) { }

            std::vector<std::string> get_controls_before() const {
                return controls_before;
            }

            std::vector<std::string> get_controls_after() const {
                return controls_after;
            }

            void to_stream(std::ostream& output) const;
            friend std::ostream& operator<<(std::ostream& output,  GlobalRule const& p) {
                p.to_stream(output);
                return output;
            }
    };

    class MultiPDS {
        std::vector<pds::pds_ptr> pdss;
        std::vector<globalrule_ptr> global_rules;
        pres::epresburger_ptr constraint;
        int reversals;
        int context_switches;

        public:
            MultiPDS() { }

            MultiPDS(std::vector<pds::pds_ptr> new_pdss,
                     std::vector<globalrule_ptr> new_global_rules,
                     pres::epresburger_ptr new_constraint,
                     int new_reversals,
                     int new_context_switches) 
                : pdss(new_pdss),
                  global_rules(new_global_rules),
                  constraint(new_constraint),
                  reversals(new_reversals),
                  context_switches(new_context_switches) { }

            void add_pds(pds::pds_ptr pds) {
                if (pds)
                    pdss.push_back(pds);
            }

            void add_global_rule(globalrule_ptr r) {
                if (r)
                    global_rules.push_back(r);
            }

            void set_reversals(int new_reversals) {
                reversals = new_reversals;
            }

            void set_context_switches(int new_context_switches) {
                context_switches = new_context_switches;
            }

            void set_constraint(pres::epresburger_ptr new_constraint) {
                constraint = new_constraint;
            }

            std::vector<pds::pds_ptr> const& get_pdss() const {
                return pdss;
            }

            std::vector<globalrule_ptr> const& get_global_rules() const {
                return global_rules;
            }

            int get_reversals() const {
                return reversals;
            }

            int get_context_switches() const {
                return context_switches;
            }

            pres::epresburger_ptr get_constraint() const {
                return constraint;
            }

            void to_stream(std::ostream& output) const;
            friend std::ostream& operator<<(std::ostream& output,  MultiPDS const& p) {
                p.to_stream(output);
                return output;
            }

            int size() {
                int size = 0;
                for (pds_ptr pds : pdss) {
                    size += pds->get_rules().size();
                }
                return size;
            }


    };

}
#endif

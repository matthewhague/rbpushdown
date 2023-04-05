
#pragma once
#ifndef __PDS2PROLOG_H__
#define __PDS2PROLOG_H__

#include <sstream>

#include "../structures/pds.h"
#include "../structures/counterexpression.h"

#include "pdswriter.h"

namespace pdswr {

    const std::string THREAD_NAME     = "Pd";
    const std::string GLOBAL_TRANS    = "GlobalTrans";
    const std::string GLOBAL_INPUT    = "GlobalInputSym";
    const std::string GLOBAL_COUNTERS = "VarNames";
    const std::string INDENT          = "    ";
    const std::string SYNPCO          = "SynPCo";

    class PDS2Prolog : public PDSWriter {
        public:
            PDS2Prolog() { }

            virtual void to_stream(pds::multipds_ptr pds, std::ostream& output);

            // because we can't have _ at the start of a var name
            // we have to enforce it like this
            // public for some of the visitors
            static void write_var(std::string s, std::ostream& output) {
                output << "v_" << s;
            }

        private:
            void write_pds(pds::pds_ptr pds,
                           std::string const& pds_name,
                           std::ostream& output);
            void write_rule(pds::rule_const_ptr r, std::ostream& output);
            void write_rule_guard(ctrexp::counterexp_ptr cc, std::ostream& output);
            void write_globals(pds::multipds_ptr mpds,
                               std::ostream& output);

            std::string pco_name(std::string const& pds_name) const {
                return "PCo_" + pds_name;
            }
            std::string states_name(std::string const& pds_name) const {
                return "States_" + pds_name;
            }
            std::string stack_sym_name(std::string const& pds_name) const {
                return "StackSym_" + pds_name;
            }
            std::string input_sym_name(std::string const& pds_name) const {
                return "InputSym_" + pds_name;
            }
            std::string trans_name(std::string const& pds_name) const {
                return "Trans_" + pds_name;
            }
            std::string q0_name(std::string const& pds_name) const {
                return "Q0_" + pds_name;
            }
            std::string z0_name(std::string const& pds_name) const {
                return "Z0_" + pds_name;
            }
            std::string Qacc_name(std::string const& pds_name) const {
                return "Qacc_" + pds_name;
            }
            std::string declaration_name() const {
                return "synpco_generated";
            }
            std::string declaration() const {
                return declaration_name() + "(SynPCo) :-";
            }


            std::string make_thread_name(int n) {
                std::ostringstream s;
                s << THREAD_NAME << n;
                return s.str();
            }


            void write_synpco(pds::multipds_ptr mpds, std::ostream& output);
            void write_constraint(pres::epresburger_ptr constraint,
                                  std::ostream& output);
            void write_call(pds::multipds_ptr mpds, std::ostream& output);

            template<class Container>
            void write_action_list(Container const& actions,
                                   std::ostream& output) {
                output << "[";

                auto it = actions.begin();
                bool hasoutput = false;
                while (it != actions.end()) {
                    std::string const& s = *it;
                    if (s != E_ACT) {
                        if (hasoutput)
                            output << ",";
                        output << "t(";
                        write_var(s, output);
                        output << ")";
                        hasoutput = true;
                    }
                    it++;
                }

                output << "]";
            }

            void get_counters(std::set<std::pair<std::string,int>>& counters,
                              pds::multipds_ptr mpds);

            void get_freevariables(std::set<std::string>& counters,
                                   pds::multipds_ptr mpds);
   };

}
#endif

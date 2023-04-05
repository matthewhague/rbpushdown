
#include <iostream>
#include <set>

#include <boost/range/adaptor/sliced.hpp>

#include "../tools/tools.h"

#include "../structures/pds.h"
#include "../structures/counterexpression.h"

#include "pds2prolog.h"

using boost::adaptors::sliced;

using namespace std;
using namespace pds;
using namespace cexp;

using namespace pdswr;


void PDS2Prolog::to_stream(multipds_ptr mpds, ostream& output) {
    int n = 0;
    
    output << declaration() << "\n";
    write_synpco(mpds, output);
    for (pds_ptr p : mpds->get_pdss()) {
        write_pds(p, make_thread_name(n++), output);
    }
    write_globals(mpds, output);

    output << "\n\n";

    write_call(mpds, output);
}


void PDS2Prolog::write_synpco(multipds_ptr mpds, ostream& output) {
    // SynPCo = synpco(2,[PCo1,PCo2],GlobalTrans,GlobalInputSym,VarNames),
    int n = mpds->get_pdss().size();
    output << INDENT << SYNPCO << " = synpco(" << n << ",[";
    for (int i = 0; i < n; i++) {
        output << pco_name(make_thread_name(i));
        if (i < n - 1)
            output << ",";
    }
    output << "]," << GLOBAL_TRANS 
           << "," << GLOBAL_INPUT 
           << "," << GLOBAL_COUNTERS 
           << "),\n";
}



void PDS2Prolog::write_pds(pds_ptr pds, 
                           std::string const& pds_name, 
                           ostream& output) {
    // PCo = pco(States,StackSym,InputSym,Trans,(Q0,sym(Z0)),Qacc),
    output << INDENT 
           << pco_name(pds_name) 
           << " = pco("
           << states_name(pds_name) << ","
           << stack_sym_name(pds_name) << ","
           << input_sym_name(pds_name) << ","
           << trans_name(pds_name) << ","
           << "(" << q0_name(pds_name) << ",sym(" << z0_name(pds_name) << ")),"
           << Qacc_name(pds_name) << "),\n";

    // States = [...],
    output << INDENT << states_name(pds_name) << " = [";
    tools::write_list_sep(pds->get_controls(), 
                          ",\n" + INDENT + INDENT,
                          output, 
                          [&] (string const& s) {
        write_var(s, output);
    });
    output << "],\n";

    // StackSym1 = [sym(f__find_target_type_1),
    output << INDENT << stack_sym_name(pds_name) << " = [";
    tools::write_list_sep(pds->get_alphabet(),
                          ",\n" + INDENT + INDENT,
                          output, 
                          [&] (string const& s) {
        output << "sym(";
        write_var(s, output);
        output << ")";
    });
    output << "],\n";

    // InputSym1 = [t(alloc),t(free),t(list_underflow),t(expected_err),
    output << INDENT << input_sym_name(pds_name) << " = ";
    write_action_list(pds->get_actions(), output);
    output << ",\n";

    // Q01 = q0r0_1,
    output << INDENT 
           << q0_name(pds_name) 
           << " = ";
    write_var(pds->get_init_p(), output);
    output << ",\n";
    // Z_01 = init_1,
    output << INDENT 
           << z0_name(pds_name) 
           << " = ";
    write_var(pds->get_init_a(), output); 
    output << ",\n";
    // Qacc1 = err_1,
    output << INDENT 
           << Qacc_name(pds_name) 
           << " = ";
    write_var(pds->get_fin_p(), output); 
    output << ",\n";


    // Trans1 = [...]
    output << INDENT << trans_name(pds_name) << " = [";
    tools::write_list_sep(pds->get_rules(), 
                          ",\n" + INDENT + INDENT, 
                          output, 
                          [&, this] (rule_const_ptr r) {
        write_rule(r, output);
    });
    output << "],\n";
}

void PDS2Prolog::write_rule(rule_const_ptr r, ostream& output) {
    // tran(from(q,[sym(a)],gt([counter],[0])),[act],to(q',[sym(bot),sym(newtop)],[(counter,inc)]))
    output << "tran(";

    output << "from(";
    write_var(r->get_p(), output);
    output << ","
           << "[sym(";
    write_var(r->get_a(), output);
    output << ")],";
    counterexp_ptr cc = r->get_guard();
    write_rule_guard(cc, output);
    output << "),";

    write_action_list(r->get_actions(), output);
    output << ",";

    output << "to(";
    write_var(r->get_q(), output);
    output << ","
           << "[";
    vector<string> w = r->get_w();
    auto it = w.rbegin();
    while(it != w.rend()) {
        output << "sym(";
        write_var(*it, output);
        output << ")";
        if (++it != w.rend())
            output << ",";
    }
    output << "],[";
    tools::write_list(r->get_counter_acts(), 
               output, 
               [&] (pair<string, int> const& act) {
        output << "(";
        write_var(act.first, output);
        output << ", " << act.second << ")";
    });
    output << "])";
    
    output << ")";
}


void PDS2Prolog::write_rule_guard(counterexp_ptr cc, ostream& output) {
    class ExpressionWriter : public CExpVisitor {
        ostream* output;

        public:
            void to_stream(counterexp_ptr cc, ostream& the_output) {
                output = &the_output;
                cc->accept(*this);
            }
            
            virtual void visit(CExpConst& b) { 
                if (b.get_value())
                    *output << "true";
                else 
                    *output << "false";
            }

            virtual void visit(CExpAnd& b) { 
                *output << "and(["; 
                tools::write_list(b.get_operands(), 
                                  *output, 
                                  [this] (counterexp_ptr c) {
                    c->accept(*this);
                });
                *output << "])";
            }

            virtual void visit(CExpOr& b) { 
                *output << "or(["; 
                tools::write_list(b.get_operands(), 
                                  *output, 
                                  [this] (counterexp_ptr c) {
                    c->accept(*this);
                });
                *output << "])";
            }

            virtual void visit(CExpImplies& b) { 
                *output << "imply([";
                b.get_lhs()->accept(*this);
                *output << "],[";
                b.get_rhs()->accept(*this);
                *output << "])";
            }
            
            virtual void visit(CExpNot& b) { 
                cerr << "PDS2Prolog::write_rule_guard: not operation not supported." << endl;
                exit(-1);
            }

            virtual void visit(CExpConstCompare& b) { 
                if (b.get_op() == NEQ) {
                    *output << "or(lt([";
                    PDS2Prolog::write_var(b.get_variable_name(), *output);
                    *output << "],[" 
                            << b.get_value() 
                            << "]),gt([";
                    PDS2Prolog::write_var(b.get_variable_name(), *output);
                    *output << "],["
                            << b.get_value()
                            << "]))";
                } else {
                    switch (b.get_op()) {
                        case EQ:  *output << "eq"; break;
                        case LT:  *output << "lt"; break;
                        case GT:  *output << "gt"; break;
                        case LTE: *output << "leq"; break;
                        case GTE: *output << "geq"; break;
                        case NEQ: /* done above */; break;
                    }
                    *output << "([";
                    PDS2Prolog::write_var(b.get_variable_name(), *output);
                    *output << "],[" << b.get_value() << "])";
                }
            }

            virtual void visit(CExpFVCompare& b) { 
                if (b.get_op() == NEQ) {
                    *output << "or(lt([";
                    PDS2Prolog::write_var(b.get_variable_name(), *output);
                    *output << "],[";
                    PDS2Prolog::write_var(b.get_freevariable_name(), *output);
                    *output << "]),gt([";
                    PDS2Prolog::write_var(b.get_variable_name(), *output);
                    *output << "],[";
                    PDS2Prolog::write_var(b.get_freevariable_name(), *output);
                    *output << "]))";
                } else {
                    switch (b.get_op()) {
                        case EQ:  *output << "eq"; break;
                        case LT:  *output << "lt"; break;
                        case GT:  *output << "gt"; break;
                        case LTE: *output << "leq"; break;
                        case GTE: *output << "geq"; break;
                        case NEQ: /* done above */; break;
                    }
                    *output << "([";
                    PDS2Prolog::write_var(b.get_variable_name(), *output);
                    *output << "],[";
                    PDS2Prolog::write_var(b.get_freevariable_name(), *output);
                    *output << "])";
                }
            }
    } expwriter;
    if (cc) {
        cexp::counterexp_ptr cc_norm = cc->normal_form(false);
        expwriter.to_stream(cc_norm, output);
    } else output << "true";
}

void PDS2Prolog::write_globals(pds::multipds_ptr mpds, 
                               std::ostream& output) {
    output << INDENT << GLOBAL_INPUT << " = [],\n";

    set<pair<string,int>> counters;
    get_counters(counters, mpds);
    set<string> freevariables;
    get_freevariables(freevariables, mpds);

    if (freevariables.size() > 0) {
        output << "FREE VARIABLES: ";
        for(string const& c : freevariables) {
            write_var(c, output);
            output << " ";
        }
        output << "\n";
    }

    output << INDENT << GLOBAL_COUNTERS << " = [";
    tools::write_list(counters, output, [&] (pair<string, int> const& s) {
        write_var(s.first, output);
    });
    output << "],\n";


    output << INDENT << GLOBAL_TRANS << " = [";
    tools::write_list_sep(mpds->get_global_rules(), 
                          ",\n" + INDENT + INDENT,
                          output, 
                          [&] (globalrule_ptr r) {
        output << "tran(from(states(";
        tools::write_list(r->get_controls_before(), 
                          output, 
                          [&] (string const& s) {
            write_var(s, output);
        });
        output << "),true),[],to(states(";
        tools::write_list(r->get_controls_after(), 
                          output, 
                          [&] (string const& s) {
            write_var(s, output);
        });
        output << "),[]))";
    });
    output << "].\n";
}



void PDS2Prolog::write_constraint(pres::epresburger_ptr constraint,
                                  ostream& output) {
    class ExpWriter : public pres::EPresVisitor, 
                      public pres::EPresValVisitor {
        ostream& output;

        public:
            ExpWriter(ostream& new_output) :
                output(new_output) { }

            void write(pres::epresburger_ptr fmla) {
                fmla->accept(*this);
            }

            virtual void visit(pres::EPresCompare& obj) {
                if (obj.get_op() == NEQ) {
                    output << "or(lt([";
                    obj.get_lhs()->accept(*this);
                    output << "],[";
                    obj.get_rhs()->accept(*this);
                    output << "]),gt([";
                    obj.get_lhs()->accept(*this);
                    output << "],[";
                    obj.get_rhs()->accept(*this);
                    output << "]))";
                } else {
                    switch (obj.get_op()) {
                        case pres::EQ: output << "eq(["; break;
                        case pres::NEQ: /* done above */; break;
                        case pres::LT: output << "lt(["; break;
                        case pres::GT: output << "gt(["; break;
                        case pres::LTE: output << "lte(["; break;
                        case pres::GTE: output << "gte(["; break;
                    }
                    obj.get_lhs()->accept(*this);
                    output << "],[";
                    obj.get_rhs()->accept(*this);
                    output << "])";
                }
            }

            virtual void visit(pres::EPresImplies& obj) {
                output << "imply([";
                obj.get_lhs()->accept(*this);
                output << "],[";
                obj.get_rhs()->accept(*this);
                output << "])";
            }

            virtual void visit(pres::EPresAnd& obj) {
                output << "and([";
                tools::write_list(obj.get_operands(), 
                                  output,
                                  [&, this] (pres::epresburger_ptr p) {
                    p->accept(*this);
                });
                output << "])";
            }

            virtual void visit(pres::EPresOr& obj) {
                output << "or([";
                tools::write_list(obj.get_operands(), 
                                  output,
                                  [&, this] (pres::epresburger_ptr p) {
                    p->accept(*this);
                });
                output << "])";
            }

            virtual void visit(pres::EPresNot& obj) {
                cerr << "PDS2Prolog::write_constraint(): Negation is currently not supported." << endl;
                exit(-1);
//                output << "not([";
//                obj.get_expression()->accept(*this);
//                output << "])";
            }

            virtual void visit(pres::EPresExists& obj) {
                cerr << "PDS2Prolog::write_constraint(): EPresExists object not supported at present."
                     << endl;
                exit(-1);
            }

            virtual void visit(pres::EPresConst& obj) {
                if (obj.get_value())
                    output << "true";
                else
                    output << "false";
            }

            virtual void visit(pres::EPresVar& obj) {
                output << "t(";
                write_var(obj.get_name(), output);
                output << ")";
            }

            virtual void visit(pres::EPresPlus& obj) {
                tools::write_list(obj.get_operands(), 
                                  output,
                                  [&, this] (pres::epres_val_ptr p) {
                    p->accept(*this);
                });
            }

            virtual void visit(pres::EPresConstMult& obj) {
                output << obj.get_scalar() 
                       << "*";
                obj.get_scalee()->accept(*this);
            }

            virtual void visit(pres::EPresInteger& obj) {
                output << obj.get_value();
            }

            virtual void visit(pres::EPresMinus& obj) {
                cerr << "PDS2Prolog::write_constraint: Subtraction is currently not supported by synpco."
                     << endl;
                exit(-1);
//                output << "sub([";
//                obj.get_lhs()->accept(*this);
//                output << "],[";
//                obj.get_rhs()->accept(*this);
//                output << "])";
            }

    } expwriter(output);
    expwriter.write(constraint);
}



void PDS2Prolog::write_call(pds::multipds_ptr mpds, std::ostream& output) {
    output << "gencall_alt:-\n"
           << INDENT << "assert(synpco_optimize_flag),\n"
           << INDENT << declaration_name() + "(SynPCo),\n"
           << INDENT << "isvalid_synpco(SynPCo),\n"
           << INDENT << "open('output.txt',write,OS),\n"
           << INDENT << "ParikhConstraint = ";

    if (mpds->get_constraint()) {
        pres::epresburger_ptr norm = mpds->get_constraint()->normal_form(false);
        write_constraint(norm, output);
    } else
        output << "true";
    output << ",\n";

    set<pair<string,int>> counters;
    get_counters(counters, mpds);

    output << INDENT << "RevBound = [";
    tools::write_list(counters, output, [&] (pair<string,int> const& s) {
        write_var(s.first, output);
        output << "-" << s.second;
    });
    output << "],\n";

    output << INDENT << "ConBound = " << mpds->get_context_switches() << ",\n"
           << INDENT << "write('Starting Translation'), nl,\n"
           << INDENT << "cbreach_synpco_to_z3_new2(SynPCo,RevBound,ConBound,ParikhConstraint,OS),\n"
           << INDENT << "close(OS).\n\n\n";



    output << "gencall:-\n"
           << INDENT << "assert(synpco_optimize_flag),\n"
           << INDENT << declaration_name() + "(SynPCo),\n"
           << INDENT << "isvalid_synpco(SynPCo),\n"
           << INDENT << "open('output.txt',write,OS),\n"
           << INDENT << "ParikhConstraint = ";

    if (mpds->get_constraint()) {
        pres::epresburger_ptr norm = mpds->get_constraint()->normal_form(false);
        write_constraint(norm, output);
    } else
        output << "true";
    output << ",\n";

//    set<string> counters;
//    get_counters(counters, mpds);

    output << INDENT << "RevBound = [";
    tools::write_list(counters, output, [&] (pair<string,int> const& s) {
        write_var(s.first, output);
        output << "-" << s.second;
    });
    output << "],\n";

    output << INDENT << "ConBound = " << mpds->get_context_switches() << ",\n"
           << INDENT << "write('Starting Translation'), nl,\n"
           << INDENT << "cbreach_synpco_to_z3_new(SynPCo,RevBound,ConBound,ParikhConstraint,OS),\n"
           << INDENT << "close(OS).\n";

}


void PDS2Prolog::get_counters(set<pair<string,int>>& counters, 
                              multipds_ptr mpds) {
    for (pds_ptr p : mpds->get_pdss()) {
        for (string const& a : p->get_counters()) {
            counters.insert(make_pair(a, p->get_reversals(a)));
        }
    }
}

void PDS2Prolog::get_freevariables(set<string>& counters, 
                                   multipds_ptr mpds) {
    for (pds_ptr p : mpds->get_pdss()) {
        for (string const& a : p->get_freevariables()) {
            counters.insert(a);
        }
    }
}


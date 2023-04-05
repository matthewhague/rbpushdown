


#include "pres2smt.h"

#include "../structures/epresburger.h"

#include <ostream>
#include <set>

using namespace std;
using namespace pres;

#define VARS_PER_ROW_IN_PLUS 4

void Pres2SMT::to_stream(pres_ptr& fmla, ostream& output_stream) {
    set<string> vars;
    bool good = get_unique_vars(fmla, vars);
    if (!good) {
        cerr << "Pres2SMT::to_stream requires the formula to have "
             << "no variable name clashes between existential statements."
             << "\n Clash on: ";
        for (auto it = vars.begin(); it != vars.end(); ++it) {
            cerr << *(it) << " ";
        }
        cerr << endl;
        exit(-1);
    } else {
        output = &output_stream;

        *output << "(benchmark pco\n"
                << ":logic QF_LIA\n" 
                << ":extrafuns( ";
        increase_indent(12);
        auto it = vars.begin();
        while (it != vars.end()) {
            *output << "( " << *(it) << " Int )";
            if (++it != vars.end()) {
                write_indent();
            }
        }
        *output << " )\n";
        decrease_indent(12);
        write_fmla_command();
        *output << "\n(and ";
        increase_indent(5);
        for (auto it = vars.begin(); it != vars.end(); ++it) {
            *output << "(<= 0 " << *(it) << ")";
            write_indent();
        }
        fmla->accept(*this);
        *output << "))";
    }
}

void Pres2SMT::visit(EPLessThan& obj) {
    string op("<");
    do_compare(obj, op);
}

void Pres2SMT::visit(EPLessThanEq& obj) {
    string op("<=");
    do_compare(obj, op);
}

void Pres2SMT::visit(EPEqual& obj) {
    string op("=");
    do_compare(obj, op);
}

void Pres2SMT::do_compare(EPCompare const& obj, string const& op) {
    *output << "(" << op << " ";
    int indent = op.length() + 2;
    increase_indent(indent);
    obj.get_lhs()->accept(*this);
//    write_indent();
    *output << " ";
    obj.get_rhs()->accept(*this);
    *output << ")";
    decrease_indent(indent);
}

void Pres2SMT::visit(EPImplies& obj) {
    *output << "(or (not ";
    increase_indent(9);
    obj.get_lhs()->accept(*this);
    *output << ")";
    decrease_indent(5);
    write_indent();
    obj.get_rhs()->accept(*this);
    *output << ")";
    decrease_indent(4);
}

void Pres2SMT::visit(EPConj& obj) {
    string a("and");
    string t("true");
    do_assoc_bin(a, t, obj);
}

void Pres2SMT::visit(EPDisj& obj) {
    string o("or");
    string f("false");
    do_assoc_bin(o, f, obj);
}

void Pres2SMT::do_assoc_bin(string const& connective, string const& def, EPAssocBin const& obj) {
    vector<pres_ptr> const& clauses = obj.get_clauses();
    if (clauses.size() == 0) {
        *output << def;
    } else if (clauses.size() == 1) {
        (*clauses.begin())->accept(*this);
    } else {
        int indent = connective.length() + 2;
        *output << "(" << connective << " ";
        increase_indent(indent);
        auto it = clauses.begin();
        while (it != clauses.end()) {
            (*it)->accept(*this);
            if (++it != clauses.end()) {
                write_indent();
            }
        }
        *output << ")";
        decrease_indent(indent);
    }
}

void Pres2SMT::visit(EPNeg& obj) {
    increase_indent(5);
    *output << "(not ";
    obj.get_fmla()->accept(*this);
    *output << ")";
    decrease_indent(5);
}

void Pres2SMT::visit(EPExists& obj) {
    obj.get_fmla()->accept(*this);
}

void Pres2SMT::visit(EPBool& obj) {
    *output << (obj.get_value() ? "true" : "false");
}


void Pres2SMT::visit(EPVar& obj) {
    *output << obj;
}

void Pres2SMT::visit(EPPlus& obj) {
    vector<pres_val_ptr> const& ops = obj.get_operands();
    if (ops.size() == 0) {
        cout << "0";
    } else if (ops.size() == 1) {
        (*ops.begin())->accept(*this);
    } else {
        *output << "(+ ";
        increase_indent(3);
        auto it = ops.begin();
        int count = 0;
        while (it != ops.end()) {
            (*it)->accept(*this);
            if (++it != ops.end()) {
                if (++count%VARS_PER_ROW_IN_PLUS == 0) 
                    write_indent();
                else
                    *output << " ";
            }
        }
        *output << ")";
        decrease_indent(3);
    }
}

void Pres2SMT::visit(EPConstMult& obj) {
    *output << "(* " << obj.get_scalar() << " ";
    obj.get_scalee()->accept(*this);
    *output << ")";
}

void Pres2SMT::visit(EPInteger& obj) {
    *output << obj;
}

void Pres2SMT::visit(EPMinus& obj) {
    *output << "(- ";
    increase_indent(3);
    obj.get_lhs()->accept(*this);
    write_indent();
    obj.get_rhs()->accept(*this);
    *output << ")";
    decrease_indent(3);
}



bool Pres2SMT::get_unique_vars(pres_ptr fmla, set<string>& vars) {
   class VarScanner : public pres::EPresburgerVisitor {
        // after a call to recurse, all_vars should contain all vars in 
        // fmla, except if the clash flag is set, then holds clash var.
        set<string>* all_vars;
        bool clash;


        public:
            bool unique_vars(pres_ptr& fmla, set<string>& vars) {
                all_vars = &vars;
                all_vars->clear();
                clash = 0;
                recurse(fmla);
                return !clash;
            }

            virtual void visit(EPLessThan& obj) { }
            virtual void visit(EPLessThanEq& obj) { }
            virtual void visit(EPEqual& obj) { } 
            virtual void visit(EPBool& obj) { }
            virtual void visit(EPConj& obj) { do_assoc_bin(obj); }
            virtual void visit(EPDisj& obj) { do_assoc_bin(obj); }
            virtual void visit(EPNeg& obj) { recurse(obj.get_fmla()); }

            virtual void visit(EPImplies& obj) {
                recurse(obj.get_lhs());
                if (!clash) {
                    recurse(obj.get_rhs());
                }
            }

            virtual void visit(EPExists& obj) {
                recurse(obj.get_fmla());
                if (!clash) {
                    set<string> const& elems = obj.get_vars();
                    auto it = elems.begin(); 
                    while (!clash && it != elems.end()) {
                        auto ins_res = all_vars->insert(*it);
                        clash &= ins_res.second;
                        if (clash) {
                            all_vars->clear();
                            all_vars->insert(*it);
                        }
                        ++it;
                    }
                }
            }

        private:
            void recurse(pres_ptr const& fmla) {
                fmla->accept(*this);
            }

            void do_assoc_bin(EPAssocBin const& obj) {
                vector<pres_ptr> const& clauses = obj.get_clauses();
                auto it = clauses.begin();
                while(!clash && (it != clauses.end())) {
                    recurse(*(it++));
                }
            }
   };

   VarScanner v;
   return v.unique_vars(fmla, vars);
}


void Pres2SMTPretty::write_indent() {
    *output << "\n";
    for (int i = 0; i < indent; ++i) {
        *output << " ";
    }
}


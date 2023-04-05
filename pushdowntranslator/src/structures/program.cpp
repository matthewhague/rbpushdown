
#include <cmath>

#include "../tools/tools.h"

#include "program.h"

using namespace std;
using namespace prog;
using namespace ctrexp;

int Statement::next_id = 0;

void Program::to_stream(ostream& output) const {
    output << "Init Procedures: ";
    for (procedure_ptr m : get_init_procedures()) {
        output << m->get_name() << " ";
    }
    output << "\n";

    output << "Global Vars: ";
    for (string const& v : global_vars) {
        output << v << " ";
    }
    output << "\n";

    output << "Counters: ";
    for (string const& c : counters) {
        output << "(" << c << ", " << get_reversals(c) << ") ";
    }
    output << "\n";

    if (constraint)
        output << "Constraint: " << *constraint << "\n";
    else
        output << "Constraint: none.\n";

    for (procedure_ptr const& m : procedures) {
        output << "\n" << (*m) << "\n";
    }
}


int Program::get_reversals(std::string const& counter) const {
    auto it = counter_revs.find(counter);
    if (it == counter_revs.end()) {
        cerr << "Program::get_reversals has no reversals setting for counter "
             << counter
             << endl;
        exit(-1);
    }
    return it->second;
}



void Procedure::to_stream(ostream& output) const {
    output << "procedure " << name << "(";
    auto it = arguments.begin();
    while (it != arguments.end()) {
        output << "bool " << (*it);
        if (++it != arguments.end()) {
            output << ", ";
        }
    }
    output << ")\n";

    for (string const& s : locals) {
        output << TAB << "bool " << s << "\n";
    }

    if (statement)
        statement->to_stream(output, TAB);
    else
        output << TAB << "No statements.\n";
}


void StatementBlock::to_stream(ostream& output, string const& indent) const {
    output_indent_labels(output, indent);
    output << "begin\n";
    for (statement_ptr s : statements) {
        s->to_stream(output, indent + TAB);
        output << "\n";
    }
    output << indent << "end;";
}

void StatementAssign::to_stream(ostream& output, string const& indent) const {
    output_indent_labels(output, indent);
    auto it = assigns.begin();
    while (it != assigns.end()) {
        output << it->first << " = ";
        if (it->second)
            output << (*it->second);
        else
            output << "no rhs";
        it++;
        if (it != assigns.end())
            output << ", ";
    }
    output << ";";
}

void StatementIf::to_stream(ostream& output, string const& indent) const {
    output_indent_labels(output, indent);

    output << "if ";

    if (is_nondet())
        output << "??";
    else {
        if (var_conditional)
            output << "{" << (*var_conditional) << "}";
        if (counter_conditional)
            output << "[" << (*counter_conditional) << "]";
    }

    output << " then\n";
    if (then_stmt)
        then_stmt->to_stream(output, indent + TAB);
    else
        output << indent << TAB << "no then branch";

    if (else_stmt) {
        output << "\n" << indent << "else\n";
        else_stmt->to_stream(output, indent + TAB);
    }
}

void StatementWhile::to_stream(ostream& output, string const& indent) const {
    output_indent_labels(output, indent);

    output << "while ";

    if (is_nondet())
        output << "??";
    else {
        if (var_conditional)
            output << "{" << (*var_conditional) << "}";
        if (counter_conditional)
            output << "[" << (*counter_conditional) << "]";
    }
    output << " do\n";

    if (body)
        body->to_stream(output, indent + TAB);
    else
        output << indent << TAB << "no body";
}

void StatementCall::to_stream(ostream& output, string const& indent) const {
    output_indent_labels(output, indent);
    output << procedure_name << "(";
    auto it = arguments.begin();
    while (it != arguments.end()) {
        (*it)->to_stream(output);
        if (++it != arguments.end()) {
            output << ",";
        }
    }
    output << ");";
}

void StatementReturn::to_stream(ostream& output, string const& indent) const {
    output_indent_labels(output, indent);
    output << "return;";
}


void StatementCounterAdj::to_stream(ostream& output, string const& indent) const {
    output_indent_labels(output, indent);
    output << counter;
    if (increment < 0)
        output << " -= ";
    else
        output << " += ";
    output << abs(increment) << ";";

}



void Statement::output_indent_labels(ostream& output, string const& indent) const {
    output << indent;
    for (string const& l : labels) {
        output << l << ": ";
    }
}


void Program::add_procedure(procedure_ptr procedure) {
    if (procedure) {
        procedures.push_back(procedure);
    }
}


void StatementSwitch::to_stream(std::ostream& output, std::string const& indent) const {
    output_indent_labels(output, indent);
    output << "switch";

    for(statement_ptr const& s : get_branches()) {
        output << "\n" << indent << "case:\n";
        s->to_stream(output, indent + TAB);
    }

    output << "\n" << indent << "end;";
}


void StatementAssert::to_stream(std::ostream& output, std::string const& indent) const {
    output_indent_labels(output, indent);
    output << "assert ";
    if (var_conditional)
        output << "{" << (*var_conditional) << "}";
    if (counter_conditional)
        output << "[" << (*counter_conditional) << "]";
    output << ";";
}

void StatementLock::to_stream(std::ostream& output, std::string const& indent) const {
    output_indent_labels(output, indent);
    if (!lock)
        output << "un";
    output << "lock " << var << ";";
}


void Program::set_reversals(std::string const& counter, int reversals) {
    auto it = counter_revs.find(counter);
    if (it == counter_revs.end()) {
        counter_revs.insert(make_pair(counter, reversals));
    } else {
        it->second = reversals;
    }
}


vector<string> Program::check_consistency() {

    vector<string> errors;

    for (string s : counters) {
        if (freevariables.find(s) != freevariables.end())
            errors.push_back("variable " + s + "both counter and free variable!");
    }

    for (procedure_ptr p : procedures) {
        class Trawler : public StatementVisitorDefault,
                        public CExpVisitor {
            vector<string>& errors;
            set<string> const& counters;
            set<string> const& freevariables;

            public:
                Trawler(vector<string>& new_errors,
                        set<string> const& new_counters,
                        set<string> const& new_freevariables)
                    : errors(new_errors),
                      counters(new_counters),
                      freevariables(new_freevariables) { }

                void visit(StatementBlock& s) {
                    for (statement_ptr sub_s : s.get_statements())
                        sub_s->accept(*this);
                }

                void visit(StatementIf& s) {
                    counterexp_ptr cc = s.get_counter_conditional();
                    if (cc)
                        cc->accept(*this);

                    statement_ptr then_s = s.get_then_stmt();
                    statement_ptr else_s = s.get_else_stmt();

                    if (then_s)
                        then_s->accept(*this);
                    if (else_s)
                        else_s->accept(*this);
                }

                void visit(StatementWhile& s) {
                    counterexp_ptr cc = s.get_counter_conditional();
                    if (cc)
                        cc->accept(*this);
                    statement_ptr body = s.get_body();
                    if (body)
                        body->accept(*this);
                }

                void visit(StatementSwitch& s) {
                    for (statement_ptr sub_s : s.get_branches())
                        sub_s->accept(*this);
                }

                void visit(StatementCounterAdj& s) {
                    string const& c = s.get_counter();
                    if (counters.find(c) == counters.end())
                        errors.push_back("ident " + c +
                                         " is not a counter, but used as one!");
                }

                void visit(StatementAssert& s) {
                    s.get_counter_conditional()->accept(*this);
                }

                void visit(CExpConst& b) { }

                void visit(CExpAnd& b) {
                    for (counterexp_ptr e : b.get_operands())
                        e->accept(*this);
                }

                void visit(CExpOr& b) {
                    for (counterexp_ptr e : b.get_operands())
                        e->accept(*this);
                }

                void visit(CExpImplies& b) {
                    b.get_lhs()->accept(*this);
                    b.get_rhs()->accept(*this);
                }

                void visit(CExpNot& b) {
                    b.get_expression()->accept(*this);
                }

                void visit(CExpConstCompare& b) {
                    string const& c = b.get_variable_name();
                    if (counters.find(c) == counters.end() &&
                        freevariables.find(c) == freevariables.end())
                        errors.push_back("Counter comparison with " +
                                         c +
                                         " which is not a counter!");
                }

                void visit(CExpFVCompare& b) {
                    string const& c = b.get_variable_name();
                    if (counters.find(c) == counters.end() &&
                        freevariables.find(c) == freevariables.end())
                        errors.push_back("Counter comparison (lhs) with " +
                                         c +
                                         " which is not a counter!");

                    string const& fv = b.get_freevariable_name();
                    if (freevariables.find(fv) == freevariables.end())
                        errors.push_back("Free variable (rhs) comparison with " +
                                         fv +
                                         " which is not a free variable!");

                }


                void do_default(Statement& s) {
                    // do nothing
                }

        } trawler(errors, counters, freevariables);

        p->get_statement()->accept(trawler);
    }

    return errors;
}

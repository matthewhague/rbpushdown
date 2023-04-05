
#pragma once
#ifndef __PROGRAM_H__
#define __PROGRAM_H__

#include <set>
#include <map>
#include <vector>
#include <iostream>
#include <string>

#include "boost/shared_ptr.hpp"

#include "variableexpression.h"
#include "counterexpression.h"
#include "epresburger.h"
#include "pds.h"

namespace prog {
    std::string const TAB("    ");
    // guaranteed null stmt id
    int const NO_STMT = -1;


    class Program;
    class Procedure;
    class Statement;

    class StatementAssign;
    class StatementIf;
    class StatementWhile;
    class StatementCall;
    class StatementReturn;
    class StatementBlock;
    class StatementCounterAdj;
    class StatementEcho;
    class StatementSwitch;
    class StatementAssert;
    class StatementLock;
    class StatementGoto;

    class StatementVisitor;
    class StatementVisitorDefault;

    typedef boost::shared_ptr<Program>             program_ptr;
    typedef boost::shared_ptr<Procedure>           procedure_ptr;
    typedef boost::shared_ptr<Statement>           statement_ptr;

    typedef boost::shared_ptr<StatementAssign>     stmt_assign_ptr;
    typedef boost::shared_ptr<StatementIf>         stmt_if_ptr;
    typedef boost::shared_ptr<StatementWhile>      stmt_while_ptr;
    typedef boost::shared_ptr<StatementCall>       stmt_call_ptr;
    typedef boost::shared_ptr<StatementReturn>     stmt_return_ptr;
    typedef boost::shared_ptr<StatementBlock>      stmt_block_ptr;
    typedef boost::shared_ptr<StatementCounterAdj> stmt_counter_adj_ptr;
    typedef boost::shared_ptr<StatementEcho>       stmt_echo_ptr;
    typedef boost::shared_ptr<StatementSwitch>     stmt_switch_ptr;
    typedef boost::shared_ptr<StatementAssert>     stmt_assert_ptr;
    typedef boost::shared_ptr<StatementLock>       stmt_lock_ptr;
    typedef boost::shared_ptr<StatementGoto>       stmt_goto_ptr;

    class Program {
        // all threads share shared vars
        std::set<std::string> shared_vars;
        // each thread has its own copy of global vars
        std::set<std::string> global_vars;
        std::set<std::string> counters;
        std::set<std::string> freevariables;
        std::map<std::string,int> counter_revs;
        std::vector<procedure_ptr> procedures;
        std::vector<procedure_ptr> init_procedures;

        int context_switches;
        pres::epresburger_ptr constraint;

        public:

            Program() {
                context_switches = 0;
            }

            void set_context_switches(int new_context_switches) {
                context_switches = new_context_switches;
            }

            void set_constraint(pres::epresburger_ptr new_constraint) {
                constraint = new_constraint;
            }
            
            void add_procedure(procedure_ptr procedure);

            void add_thread(procedure_ptr run_procedure) {
                init_procedures.push_back(run_procedure);
            }

            void add_shared_var(std::string const& var) {
                shared_vars.insert(var);
            }

            void add_global_var(std::string const& var) {
                global_vars.insert(var);
            }

            void add_counter(std::string const& counter,
                             int reversals) {
                counters.insert(counter);
                set_reversals(counter, reversals);
            }

            void add_freevariable(std::string const& freevariable) {
                freevariables.insert(freevariable);
            }

            int get_reversals(std::string const& counter) const;

            void set_reversals(std::string const& counter, 
                               int reversals);

            std::vector<procedure_ptr> const& get_procedures() const {
                return procedures;
            }

            std::set<std::string> const& get_global_vars() const {
                return global_vars;
            }

            std::set<std::string> const& get_shared_vars() const {
                return shared_vars;
            }

            std::vector<procedure_ptr> const& get_init_procedures() const {
                return init_procedures;
            }

            std::set<std::string> const& get_counters() const {
                return counters;
            }

            std::set<std::string> const& get_freevariables() const {
                return freevariables;
            }

            int get_context_switches() const {
                return context_switches;
            }

            pres::epresburger_ptr get_constraint() const {
                return constraint;
            }

            void to_stream(std::ostream& output) const;
            friend std::ostream& operator<<(std::ostream& output,  Program const& p) {
                p.to_stream(output);
                return output;
            }

            // returns a list of errors if any
            std::vector<std::string> check_consistency();

    };



    class Procedure {
        std::string name;
        std::vector<std::string> arguments;
        std::vector<std::string> locals;
        statement_ptr statement; // usually a StatementBlock

        public:
            void set_name(std::string const& new_name) {
                name = new_name;
            }

            void append_argument(std::string const& argument) {
                arguments.push_back(argument);
            }

            std::vector<std::string> get_arguments() const {
                return arguments;
            }

            void add_local_var(std::string const& local) {
                locals.push_back(local);
            }

            std::vector<std::string> get_local_vars() const {
                return locals;
            }

            std::string const& get_name() const {
                return name;
            }

            void set_statement(statement_ptr new_statement) {
                statement = new_statement;
            }

            statement_ptr get_statement() {
                return statement;
            }

            void to_stream(std::ostream& output) const;
            friend std::ostream& operator<<(std::ostream& output,  Procedure const& p) {
                p.to_stream(output);
                return output;
            }

            procedure_ptr make_shared_ptr() {
                procedure_ptr p(this);
                return p;
            }
    };


    class StatementVisitor {
        public:
            virtual void visit(StatementBlock& s) = 0; 
            virtual void visit(StatementAssign& s) = 0;
            virtual void visit(StatementIf& s) = 0;
            virtual void visit(StatementWhile& s) = 0;
            virtual void visit(StatementCall& s) = 0;
            virtual void visit(StatementReturn& s) = 0;
            virtual void visit(StatementCounterAdj& s) = 0;
            virtual void visit(StatementEcho& s) = 0;
            virtual void visit(StatementSwitch& s) = 0;
            virtual void visit(StatementAssert& s) = 0;
            virtual void visit(StatementLock& s) = 0;
            virtual void visit(StatementGoto& s) = 0;
    };


    class Statement {
        std::set<std::string> labels;
        int id;

        static int next_id;

        public:
            Statement() { id = next_id++; }

            int get_id() const {
                return id;
            }
            
            void add_label(std::string const& label) {
                labels.insert(label);
            }

            void add_labels(std::set<std::string> const& new_labels) {
                for (std::string const& label : new_labels) {
                    labels.insert(label);
                }
            }


            std::set<std::string> const& get_labels() const {
                return labels;
            }

            virtual void to_stream(std::ostream& output, std::string const& indent) const = 0;
            friend std::ostream& operator<<(std::ostream& output,  Statement const& p) {
                p.to_stream(output, "");
                return output;
            }

            virtual statement_ptr make_shared_ptr() = 0;

            virtual void accept(StatementVisitor& v) = 0;

        protected:
            void output_indent_labels(std::ostream& output, std::string const& indent) const;                
    };

    class StatementBlock : public Statement {
        std::vector<statement_ptr> statements;

        public:
            StatementBlock() { }

            StatementBlock(std::vector<statement_ptr> const& new_statements) 
                : statements(new_statements) { }

            void append_statement(statement_ptr statement) {
                if (statement) 
                    statements.push_back(statement);
            }

            std::vector<statement_ptr> const& get_statements() const {
                return statements;
            }

            void to_stream(std::ostream& output, std::string const& indent) const;

            virtual statement_ptr make_shared_ptr() {
                stmt_block_ptr p(this);
                return p;
            };

            virtual void accept(StatementVisitor& v) { v.visit(*this); }
    };


    class StatementAssign : public Statement {
        std::vector<std::pair<std::string, vexp::varexpression_ptr>> assigns;
        
        public:
            StatementAssign() { }

            StatementAssign(std::string const& new_lhs,
                            vexp::varexpression_ptr new_rhs) {
                assigns.push_back(std::make_pair(new_lhs, new_rhs));
            }

            void add_assign(std::string const& new_lhs, 
                            vexp::varexpression_ptr new_rhs) {
                assigns.push_back(std::make_pair(new_lhs, new_rhs));
            }

            std::vector<std::pair<std::string, vexp::varexpression_ptr>> get_assigns() const {
                return assigns;
            }

            void to_stream(std::ostream& output, std::string const& indent) const;

            virtual statement_ptr make_shared_ptr() {
                stmt_assign_ptr p(this);
                return p;
            };

            virtual void accept(StatementVisitor& v) { v.visit(*this); }
    }; 


    class StatementIf : public Statement {
        // if both are null, then we're representing a non-det choice
        vexp::varexpression_ptr var_conditional;
        cexp::counterexp_ptr    counter_conditional;
        statement_ptr           then_stmt;
        statement_ptr           else_stmt;

        public:
            StatementIf() { }

            StatementIf(vexp::varexpression_ptr new_var_conditional,
                        cexp::counterexp_ptr new_counter_conditional,
                        statement_ptr new_then_stmt) 
                : var_conditional(new_var_conditional),
                  counter_conditional(new_counter_conditional),
                  then_stmt(new_then_stmt) { }

            StatementIf(vexp::varexpression_ptr new_var_conditional,
                        cexp::counterexp_ptr new_counter_conditional,
                        statement_ptr new_then_stmt,
                        statement_ptr new_else_stmt) 
                : var_conditional(new_var_conditional),
                  counter_conditional(new_counter_conditional),
                  then_stmt(new_then_stmt),
                  else_stmt(new_else_stmt) { }

            bool is_nondet() const {
                return !var_conditional && !counter_conditional;
            }

            void set_var_conditional(vexp::varexpression_ptr new_conditional) {
                var_conditional = new_conditional;
            }

            void set_counter_conditional(cexp::counterexp_ptr new_conditional) {
                counter_conditional = new_conditional;
            }

            void set_then_stmt(statement_ptr new_then) {
                then_stmt = new_then;
            }

            void set_else_stmt(statement_ptr new_else) {
                else_stmt = new_else;
            }
                
            vexp::varexpression_ptr get_var_conditional() const {
                return var_conditional;
            }

            cexp::counterexp_ptr get_counter_conditional() const {
                return counter_conditional;
            }

            statement_ptr get_then_stmt() const {
                return then_stmt;
            }

            statement_ptr get_else_stmt() const {
                return else_stmt;
            }

            void to_stream(std::ostream& output, std::string const& indent) const;

            virtual statement_ptr make_shared_ptr() {
                stmt_if_ptr p(this);
                return p;
            };

            virtual void accept(StatementVisitor& v) { v.visit(*this); }
    };

    class StatementWhile : public Statement {
        // if both are null, then we're representing a non-det choice
        vexp::varexpression_ptr var_conditional;
        cexp::counterexp_ptr    counter_conditional;

        statement_ptr      body;

        public:
            StatementWhile() { }

            StatementWhile(vexp::varexpression_ptr new_var_conditional,
                           cexp::counterexp_ptr new_counter_conditional,
                           statement_ptr new_body)
                : var_conditional(new_var_conditional),
                  counter_conditional(new_counter_conditional),
                  body(new_body) { }

            bool is_nondet() const {
                return !var_conditional && !counter_conditional;
            }

            void set_var_conditional(vexp::varexpression_ptr new_conditional) {
                var_conditional = new_conditional;
            }

            void set_counter_conditional(cexp::counterexp_ptr new_conditional) {
                counter_conditional = new_conditional;
            }

            void set_body(statement_ptr new_body) {
                body = new_body;
            }

            statement_ptr get_body() const {
                return body;
            }

            vexp::varexpression_ptr get_var_conditional() const {
                return var_conditional;
            }

            cexp::counterexp_ptr get_counter_conditional() const {
                return counter_conditional;
            }

            void to_stream(std::ostream& output, std::string const& indent) const;

            virtual statement_ptr make_shared_ptr() {
                stmt_while_ptr p(this);
                return p;
            };

            virtual void accept(StatementVisitor& v) { v.visit(*this); }
    };


    class StatementCall : public Statement {
        std::string procedure_name;
        std::vector<vexp::varexpression_ptr> arguments;
        
        public:
            StatementCall() { }

            StatementCall(std::string const& new_procedure_name,
                          std::vector<vexp::varexpression_ptr> const& new_arguments) 
                : procedure_name(new_procedure_name),
                  arguments(new_arguments) { }

            void set_procedure_name(std::string const& new_procedure_name) {
                procedure_name = new_procedure_name;
            }

            std::string const& get_procedure_name() const {
                return procedure_name;
            }

            void append_argument(vexp::varexpression_ptr argument) {
                if (argument) 
                    arguments.push_back(argument);
            }

            std::vector<vexp::varexpression_ptr> const& get_arguments() const {
                return arguments;
            }

            void to_stream(std::ostream& output, std::string const& indent) const;

            virtual statement_ptr make_shared_ptr() {
                stmt_call_ptr p(this);
                return p;
            };

            virtual void accept(StatementVisitor& v) { v.visit(*this); }
    };

    class StatementReturn : public Statement {
        public:
            void to_stream(std::ostream& output, std::string const& indent) const;

            virtual statement_ptr make_shared_ptr() {
                stmt_return_ptr p(this);
                return p;
            };

            virtual void accept(StatementVisitor& v) { v.visit(*this); }
    };


    class StatementCounterAdj : public Statement {
        std::string counter;
        int         increment;

        public:
            StatementCounterAdj() { }

            StatementCounterAdj(std::string const& new_counter,
                                int new_increment)
                : counter(new_counter),
                  increment(new_increment) { }

            void set_counter(std::string const& new_counter) {
                counter = new_counter;
            }
            
            void set_increment(int new_increment) {
                increment = new_increment;
            }

            std::string const& get_counter() const {
                return counter;
            }

            int get_increment() const {
                return increment;
            }


            void to_stream(std::ostream& output, std::string const& indent) const;

            virtual statement_ptr make_shared_ptr() {
                stmt_counter_adj_ptr p(this);
                return p;
            };

            virtual void accept(StatementVisitor& v) { v.visit(*this); }
    };

    class StatementEcho : public Statement {
        std::string action;

        public:

            StatementEcho(std::string const& new_act) {
                action = new_act;
            }

            void set_action(std::string const& new_act) {
                action = new_act;
            }

            std::string const& get_action() const {
                return action;
            }

            void to_stream(std::ostream& output, std::string const& indent) const {
                output_indent_labels(output, indent);
                if (action != E_ACT)
                    output << "echo " << action << ";";
                else
                    output << "skip;";
            }

            virtual statement_ptr make_shared_ptr() {
                stmt_echo_ptr p(this);
                return p;
            };

            virtual void accept(StatementVisitor& v) { v.visit(*this); }
    };

    class StatementSwitch : public Statement {
        std::vector<statement_ptr> branches;

        public:
            StatementSwitch() { }

            StatementSwitch(std::vector<statement_ptr> const& new_branches)
                : branches(new_branches) { }

            void add_branch(statement_ptr new_branch) {
                if (new_branch) 
                    branches.push_back(new_branch);
            }

            std::vector<statement_ptr> const& get_branches() const {
                return branches;
            }

            void to_stream(std::ostream& output, std::string const& indent) const;

            virtual statement_ptr make_shared_ptr() {
                stmt_switch_ptr p(this);
                return p;
            };

            virtual void accept(StatementVisitor& v) { v.visit(*this); }
    };


    class StatementAssert : public Statement {
        vexp::varexpression_ptr var_conditional;
        cexp::counterexp_ptr    counter_conditional;

        public:
            StatementAssert() { }

            StatementAssert(vexp::varexpression_ptr vc, cexp::counterexp_ptr cc)
                : var_conditional(vc),
                  counter_conditional(cc) { }

            void set_counter_conditional(cexp::counterexp_ptr cc) {
                counter_conditional = cc;
            }

            void set_var_conditional(vexp::varexpression_ptr vc) {
                var_conditional = vc;
            }

            vexp::varexpression_ptr get_var_conditional() const {
                return var_conditional;
            }

            cexp::counterexp_ptr get_counter_conditional() const {
                return counter_conditional;
            }

            void to_stream(std::ostream& output, std::string const& indent) const;

            virtual statement_ptr make_shared_ptr() {
                stmt_assert_ptr p(this);
                return p;
            };

            virtual void accept(StatementVisitor& v) { v.visit(*this); }
    };

    class StatementLock : public Statement {
        std::string var;
        bool lock;

        public:
            StatementLock() { }

            StatementLock(std::string const& new_var, bool new_lock)
                : var(new_var),
                  lock(new_lock) { }

            void set_lock(bool new_lock) {
                lock = new_lock;
            }

            bool get_lock() const { return lock; }

            void set_var(std::string const& new_var) {
                var = new_var;
            }

            std::string const& get_var() const { return var; }

            void to_stream(std::ostream& output, std::string const& indent) const;

            virtual statement_ptr make_shared_ptr() {
                stmt_lock_ptr p(this);
                return p;
            };

            virtual void accept(StatementVisitor& v) { v.visit(*this); }
    };

    class StatementGoto : public Statement {
        std::string goto_label;

        public:
            StatementGoto() { }

            StatementGoto(std::string const& new_goto_label)
                : goto_label(new_goto_label) { }

            void set_label(std::string const& new_goto_label) {
                goto_label = new_goto_label;
            }

            std::string const& get_goto_label() const { return goto_label; }

            void to_stream(std::ostream& output, std::string const& indent) const {
                output_indent_labels(output, indent);
                output << "goto " << goto_label << ";";
            }

            virtual statement_ptr make_shared_ptr() {
                stmt_goto_ptr p(this);
                return p;
            };

            virtual void accept(StatementVisitor& v) { v.visit(*this); }
    };


    class StatementVisitorDefault : public StatementVisitor {
        public:
            virtual void visit(StatementBlock& s) { do_default(s); }
            virtual void visit(StatementAssign& s) { do_default(s); }
            virtual void visit(StatementIf& s) { do_default(s); }
            virtual void visit(StatementWhile& s) { do_default(s); }
            virtual void visit(StatementCall& s) { do_default(s); }
            virtual void visit(StatementReturn& s) { do_default(s); }
            virtual void visit(StatementCounterAdj& s) { do_default(s); }
            virtual void visit(StatementEcho& s) { do_default(s); }
            virtual void visit(StatementSwitch& s) { do_default(s); }
            virtual void visit(StatementAssert& s) { do_default(s); }
            virtual void visit(StatementLock& s) { do_default(s); }
            virtual void visit(StatementGoto& s) { do_default(s); }
        protected:
            virtual void do_default(Statement& s) { };
    };


}



#endif



#pragma once
#ifndef __SKIPREMOVER_H__
#define __SKIPREMOVER_H__

#include "boost/make_shared.hpp"

#include "../structures/program.h"
#include "../structures/pds.h"

#include "codeoptimiser.h"

namespace prog {

    class SkipRemover : public CodeOptimiser,
                        public StatementVisitorDefault {
        // return values for recursive visiting
        bool is_skip;
        statement_ptr cur_stmt;
        statement_ptr result_stmt;

        public:
            SkipRemover() { }

            virtual void optimise(program_ptr prog);

            std::pair<bool, statement_ptr> do_statement(statement_ptr s);

            virtual void visit(StatementBlock& s);
            virtual void visit(StatementIf& s);
            virtual void visit(StatementWhile& s);
            virtual void visit(StatementEcho& s);
            virtual void visit(StatementSwitch& s);
            virtual void do_default(Statement& s);

        private:
            void set_result(bool new_is_skip, statement_ptr new_stmt) {
                new_stmt->add_labels(get_cur_stmt()->get_labels());
                is_skip = new_is_skip;
                result_stmt = new_stmt;
            }

            statement_ptr get_skip_stmt() {
                return boost::make_shared<StatementEcho>(E_ACT);
            }

            statement_ptr get_cur_stmt() {
                return cur_stmt;
            }

            void set_cur_stmt(statement_ptr s) {
                cur_stmt = s;
            }
    };

}

#endif

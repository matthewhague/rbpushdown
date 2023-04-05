
#include <vector>

#include "boost/make_shared.hpp"

#include "../structures/program.h"

#include "skipremover.h"

using namespace std;
using namespace prog;

void SkipRemover::optimise(program_ptr prog) {
    for (procedure_ptr m : prog->get_procedures()) {
        auto res = do_statement(m->get_statement());
        m->set_statement(res.second);
    }
}


std::pair<bool, statement_ptr> SkipRemover::do_statement(statement_ptr s) {
    statement_ptr old_cur = get_cur_stmt();
    set_cur_stmt(s);
    s->accept(*this);
    set_cur_stmt(old_cur);
    return make_pair(is_skip, result_stmt);
}


void SkipRemover::visit(StatementBlock& s) {
    stmt_block_ptr b = boost::make_shared<StatementBlock>();
    int num_non_skips = 0;
    set<string> dropped_labels;
    for (statement_ptr sub_s : s.get_statements()) {
        auto res = do_statement(sub_s);
        bool sub_skip = res.first;
        statement_ptr new_sub_s = res.second;
        if (!sub_skip) {
            new_sub_s->add_labels(dropped_labels);
            dropped_labels.clear();
            num_non_skips++;
            b->append_statement(new_sub_s);
        } else {
            for (string const& label : new_sub_s->get_labels())
                dropped_labels.insert(label);
        }
    }

    if (dropped_labels.size() > 0) {
        statement_ptr new_end = get_skip_stmt();
        new_end->add_labels(dropped_labels);
        b->append_statement(new_end);
        num_non_skips++;
    }

    if (num_non_skips == 0) 
        set_result(true, get_skip_stmt());
    else if (num_non_skips == 1) 
        set_result(false, b->get_statements().front());
    else 
        set_result(false, b);
}



void SkipRemover::visit(StatementIf& s) {
    auto res = do_statement(s.get_then_stmt());
    bool then_is_skip = res.first;
    statement_ptr then_s = res.second;
    s.set_then_stmt(then_s);

    statement_ptr else_s = s.get_else_stmt();

    if (!else_s) {
        if (then_is_skip)
            set_result(true, get_skip_stmt());
        else
            set_result(false, get_cur_stmt());
    } else {
        auto res = do_statement(else_s);
        bool else_is_skip = res.first;
        statement_ptr new_else_s = res.second;
        
        if (else_is_skip && then_is_skip) {
            set_result(true, get_skip_stmt());
        } else if (else_is_skip && !then_is_skip) {
            statement_ptr null_s;
            s.set_else_stmt(null_s);
            set_result(false, get_cur_stmt());
        } else {
            set_result(false, get_cur_stmt());
        }
    }
}

void SkipRemover::visit(StatementWhile& s) {
    auto res = do_statement(s.get_body());
    bool sub_is_skip = res.first;
    statement_ptr new_sub = res.second;
    if (s.is_nondet() && sub_is_skip) {
        set_result(true, get_skip_stmt());
    } else {
        // don't do anything clever, since while's may be a busy-wait
        s.set_body(res.second);
        set_result(false, get_cur_stmt());
    }
}


void SkipRemover::visit(StatementEcho& s) {
    set_result(s.get_action() == E_ACT, get_cur_stmt());
}


void SkipRemover::visit(StatementSwitch& s) {
    stmt_switch_ptr b = boost::make_shared<StatementSwitch>();
    bool isnt_skip = false;
    for (statement_ptr sub_s : s.get_branches()) {
        auto res = do_statement(sub_s);
        bool sub_skip = res.first;
        statement_ptr new_sub_s = res.second;
        if (!sub_skip) {
            isnt_skip = true;
            b->add_branch(new_sub_s);
        }
    }
    if (isnt_skip) {
        set_result(false, b);
    } else {
        set_result(true, get_skip_stmt());
    }

}

void SkipRemover::do_default(Statement& s) {
    set_result(false, get_cur_stmt());
}




#include <iostream>
#include <sstream>

#include "boost/shared_ptr.hpp"
#include "boost/make_shared.hpp"

#include "prog2pds.h"

#include "../structures/variableexpression.h"
#include "../structures/counterexpression.h"
#include "../structures/epresburger.h"

using namespace std;

using namespace prog2pds;
using namespace prog;
using namespace pds;

multipds_ptr Prog2PDS::translate(program_ptr the_prog) { 
    prog = the_prog;
    mpds = boost::make_shared<MultiPDS>();

    mpds->set_context_switches(the_prog->get_context_switches());

    // build pdss
    int n = 0;
    for (procedure_ptr m : the_prog->get_init_procedures()) {
        // if the thread will ever be run
        if (n == 0 || prog->get_context_switches() > 0) {
            string name = make_thread_name(n);
            pds_ptr p = translate_thread(m, name, n == 0);
            if (prog->get_context_switches() > 0)
                add_wait_controls(p, name);
            mpds->add_pds(p);
        }
        n++;
    }

    if (prog->get_context_switches() > 0) {
        add_global_transitions();
    }

    add_constraint();

    return mpds;
}

void Prog2PDS::add_wait_controls(pds_ptr p, string const& name) {
    // add waits to pdss
    wait_env->forall_vals([&] (vexp::Environment& e) {
        string q = make_wait_control(e, name);
        pds->add_control(q);
    });


}


std::string Prog2PDS::make_thread_name(int n) {
    std::ostringstream s;
    s << THREAD_PREFIX << n++;
    return s.str();
}


pds_ptr Prog2PDS::translate_thread(prog::procedure_ptr new_init_procedure,
                                   std::string thread_name,
                                   bool init_scheduled) {
    reset_translator();

    pds_name = thread_name;
    init_procedure = new_init_procedure;
    pds  = boost::make_shared<Pds>();

    add_counters_to_pds();
    build_maps();
    do_statements();
    add_init_and_fin(init_scheduled);

    return pds;
}

void Prog2PDS::add_counters_to_pds() {
    for (string const& c : prog->get_counters()) {
        pds->add_counter(c, prog->get_reversals(c));
    }
    for (string const& c : prog->get_freevariables()) {
        pds->add_freevariable(c);
    }
}

void Prog2PDS::reset_translator() {
    m_stmt_info.clear();
    m_label_ids.clear();
    m_name_procedure.clear();
    global_env.reset();
    init_procedure.reset();
    pds.reset();
    pds_name = "";
    m_head_info.clear();
    worklist.clear();
    reachedlist.clear();
    get_var_scope_memo.clear();
}


void Prog2PDS::add_init_and_fin(bool init_scheduled) {
    int init_stmt_id = init_procedure->get_statement()->get_id();
    vexp::environment_ptr& env = get_stmt_info(init_stmt_id).env;
    env->restore_from_hash(vexp::NULL_HASH);

    string init_stack    = make_stack(init_stmt_id, *env);
    string sbot_init     = make_stack_bot_init();
    string sbot_done     = make_stack_bot_done();
    string sbot_end      = make_stack_bot_end();
    string final_control = make_final();

    pds->add_alphabet(sbot_init);
    pds->add_alphabet(sbot_done);
    pds->add_alphabet(sbot_end);
    pds->add_alphabet(init_stack);
    pds->add_control(final_control);

    pds->set_init_a(sbot_init);
    pds->set_fin_p(final_control);

    shared_env->forall_vals([&] (vexp::Environment& se) {
        env->restore_from_hash(vexp::NULL_HASH);
        env->set_from(se);

        string init_control = make_control(*env);
        pds->add_control(init_control);

        cexp::counterexp_ptr null_cc;
        // init_control sbot -> init_control init_stack sbot
        add_push(init_control, sbot_init, E_ACT, init_control, init_stack, sbot_done, null_cc);

        // null env is init 
        if (env->get_hash() == vexp::NULL_HASH) {
            if (init_scheduled)
                pds->set_init_p(init_control);
            else
                pds->set_init_p(make_wait_control(*env));
        }

        // all exits to final state
        HeadInfo const& init_info = getadd_head_info(make_pair(init_stmt_id, env->get_hash()));
        for (int exit_hash: init_info.return_hashes) {
            global_env->restore_from_hash(exit_hash);

            string exit_control = make_control(*global_env);
            add_rewrite(exit_control, sbot_done, make_done_action(), exit_control, sbot_end, null_cc);
            add_rewrite(exit_control, sbot_end, E_ACT, final_control, sbot_end, null_cc);

            string exit_by_switch_control = make_control(*global_env, true);
            add_rewrite(exit_control, 
                        sbot_done, 
                        make_done_action(), 
                        exit_by_switch_control, 
                        sbot_end, 
                        null_cc);
        }
    });

}


void Prog2PDS::build_maps() {
    class Trawler : public StatementVisitorDefault {
        Prog2PDS& daddy;
        vexp::environment_ptr procedure_env;

        // variables for naming statements
        int next;
        std::string cur_name;
        std::string base;

        // so we can build maps of non-obvious stmt->nextstmt
        int continuation_stmt_id;

        public:
            Trawler(Prog2PDS& new_daddy) : daddy(new_daddy) { 
                reset_base("default");
                continuation_stmt_id = prog::NO_STMT;
            }

            void set_environment(vexp::environment_ptr env) {
                procedure_env = env;
            }

            void set_continuation_stmt_id(int id) {
                continuation_stmt_id = id;
            }

            int get_continuation_stmt_id() const {
                return continuation_stmt_id;
            }

            void visit(StatementBlock& s) {
                // don't increment name for stmtblk
                // it's should have same name as first statement in it
                daddy.name_statement(s, get_name());
                daddy.register_labels(s);
                daddy.set_statement_env(s, procedure_env);
                daddy.set_statement_can_switch(s, false, false);

                // add to next statement blocks
                int blk_cont = get_continuation_stmt_id();

                vector<statement_ptr> const& stmts = s.get_statements();
                auto it = stmts.begin();
                while (it != stmts.end()) {
                    statement_ptr sub_s = *it++;
                    daddy.register_stmt_ptr(sub_s);
                    if (it != stmts.end()) {
                        set_continuation_stmt_id((*it)->get_id());
                    } else {
                        set_continuation_stmt_id(blk_cont);
                    }
                    sub_s->accept(*this);
                }

                // reset for nothing but nice stack discipline
                set_continuation_stmt_id(blk_cont);
            }

            // doesn't need to register a successor since it knows them from
            // then and else pointers, unless else is null, also note that the
            // continuation id of both then and else is the continuation id of
            // the if stmt
            void visit(StatementIf& s) {
                unique_name_statement(s);
                daddy.set_statement_env(s, procedure_env);
                daddy.register_labels(s);

                bool can_switch = (daddy.is_shared(s.get_var_conditional()) || 
                                   daddy.is_shared(s.get_counter_conditional()));
                daddy.set_statement_can_switch(s, can_switch, false);

                statement_ptr then_s = s.get_then_stmt();
                statement_ptr else_s = s.get_else_stmt();

                if (then_s) {
                    daddy.register_stmt_ptr(then_s);
                    then_s->accept(*this);
                } else {
                    cerr << "Namer::visit(StatementIf& s) found a null then branch." << endl;
                    exit(-1);
                }

                if (else_s) {
                    daddy.register_stmt_ptr(else_s);
                    else_s->accept(*this);
                } else {
                    // in this case the else successor is given by continuation
                    daddy.register_successor(s.get_id(), get_continuation_stmt_id());
                }
            }

            void visit(StatementWhile& s) {
                unique_name_statement(s);
                daddy.set_statement_env(s, procedure_env);
                daddy.register_labels(s);

                bool can_switch = (daddy.is_shared(s.get_var_conditional()) || 
                                   daddy.is_shared(s.get_counter_conditional()));
                daddy.set_statement_can_switch(s, can_switch, false);

                // for when we break the loop
                daddy.register_successor(s.get_id(), get_continuation_stmt_id());

                int cont = get_continuation_stmt_id();
                // translate stmt, returning to condition after as continuation
                set_continuation_stmt_id(s.get_id());

                statement_ptr body = s.get_body();
                if (body) {
                    daddy.register_stmt_ptr(body);
                    body->accept(*this);
                } else {
                    cerr << "Namer::visit(StatementIf& s) found a null then branch." << endl;
                    exit(-1);
                }

                // reset continuation for propriety 
                set_continuation_stmt_id(cont);
            }

            void visit(StatementSwitch& s) {
                unique_name_statement(s);
                daddy.register_labels(s);
                daddy.set_statement_env(s, procedure_env);
                daddy.set_statement_can_switch(s, false, false);

                for (statement_ptr sub_s : s.get_branches()) {
                    daddy.register_stmt_ptr(sub_s);
                    sub_s->accept(*this);
                }
            }

            void visit(StatementAssign& s) {
                unique_name_statement(s);
                daddy.register_labels(s);
                daddy.register_successor(s.get_id(), get_continuation_stmt_id());
                daddy.set_statement_env(s, procedure_env);

                std::vector<std::pair<std::string, 
                                      vexp::varexpression_ptr>> assigns = s.get_assigns();
               
                bool can_switch_pre = false;
                bool can_switch_post = false;
                auto it = assigns.begin();
                
                while(!can_switch_pre && 
                      !can_switch_post && 
                      it != assigns.end()) {
                    string const& var = it->first;
                    vexp::varexpression_ptr exp = it->second;

                    can_switch_pre = daddy.is_shared(var) || daddy.is_shared(exp);
                    can_switch_post = daddy.is_shared(var);

                    ++it;
                }

                daddy.set_statement_can_switch(s, can_switch_pre, can_switch_post);
            }

            void visit(StatementCounterAdj& s) {
                unique_name_statement(s);
                daddy.register_labels(s);
                daddy.register_successor(s.get_id(), get_continuation_stmt_id());
                daddy.set_statement_env(s, procedure_env);
                daddy.set_statement_can_switch(s, true, true);
            }

            void visit(StatementCall& s) {
                unique_name_statement(s);
                daddy.register_labels(s);
                daddy.register_successor(s.get_id(), get_continuation_stmt_id());
                daddy.set_statement_env(s, procedure_env);

                bool can_switch = false;

                auto it = s.get_arguments().begin();
                auto itend = s.get_arguments().end();
                while(!can_switch && it != itend) {
                    can_switch = daddy.is_shared(*it);
                    ++it;
                }

                daddy.set_statement_can_switch(s, can_switch, false);
            }

            void visit(StatementAssert& s) {
                unique_name_statement(s);
                daddy.register_labels(s);
                daddy.register_successor(s.get_id(), get_continuation_stmt_id());
                daddy.set_statement_env(s, procedure_env);

                bool can_switch = daddy.is_shared(s.get_counter_conditional()) ||
                                  daddy.is_shared(s.get_var_conditional());
                daddy.set_statement_can_switch(s, can_switch, false);
            }

            void visit(StatementLock& s) {
                unique_name_statement(s);
                daddy.register_labels(s);
                daddy.register_successor(s.get_id(), get_continuation_stmt_id());
                daddy.set_statement_env(s, procedure_env);

                bool can_switch = daddy.is_shared(s.get_var());
                daddy.set_statement_can_switch(s, can_switch, can_switch);
            }

            void visit(StatementGoto& s) {
                unique_name_statement(s);
                daddy.register_labels(s);
                daddy.set_statement_env(s, procedure_env);
                daddy.set_statement_can_switch(s, false, false);
            }

            void visit(StatementReturn& s) {
                unique_name_statement(s);
                daddy.register_labels(s);
                daddy.set_statement_env(s, procedure_env);
                daddy.set_statement_can_switch(s, false, false);
            }

            // usually next stmt is just one given by continuation id
            void do_default(Statement& s) {
                unique_name_statement(s);
                daddy.register_labels(s);
                daddy.register_successor(s.get_id(), get_continuation_stmt_id());
                daddy.set_statement_env(s, procedure_env);
                daddy.set_statement_can_switch(s, false, false);
            }

            void unique_name_statement(Statement& s) {
                daddy.name_statement(s, get_name());
                next_name();
            }

            std::string& reset_base(std::string const& new_base) { 
                next = 1; 
                base = new_base; 
                set_name(0); 
                return get_name();
            }

        private:
            std::string& get_name() { return cur_name; }

            void next_name() { set_name(next++); }

            void set_name(int i) { 
                std::ostringstream s; 
                s << base << "_" << i; 
                cur_name = s.str(); 
            } 


    } trawler(*this);

    global_env = make_global_environment();
    shared_env = make_shared_environment();
    wait_env   = make_wait_environment();
    for (procedure_ptr m : prog->get_procedures()) {
        trawler.reset_base(m->get_name());
        add_name_procedure(m);
        statement_ptr s = m->get_statement();
        register_stmt_ptr(s);
        vexp::environment_ptr env = make_procedure_environment(m);
        trawler.set_environment(env);
        s->accept(trawler);
    }
}



void Prog2PDS::name_statement(Statement const& s, std::string const& name) {
    StatementInfo& info = getadd_stmt_info(s.get_id());
    info.name = name;
}


void Prog2PDS::register_labels(Statement const& s) {
    for (string const& label : s.get_labels()) {
        auto it = m_label_ids.find(label);
        if (it != m_label_ids.end()) {
            cerr << "Prog2PDS::register_labels found duplicate label " 
                 << label << endl;
            exit(-1);
        }
        m_label_ids.insert(make_pair(label,s.get_id()));
    }
}


int Prog2PDS::get_label_id(string const& label) {
    auto it = m_label_ids.find(label);
    if (it == m_label_ids.end()) {
        cerr << "Prog2PDS::get_label_id: " << label << " does not exist."
             << endl;
        exit(-1);
    }
    return it->second;
}


StatementInfo& Prog2PDS::getadd_stmt_info(int stmt_id) {
    auto it = m_stmt_info.find(stmt_id);
    if (it == m_stmt_info.end()) {
        StatementInfo empty;
        it = m_stmt_info.insert(make_pair(stmt_id, empty)).first;
    }
    return it->second;
}

StatementInfo& Prog2PDS::get_stmt_info(int stmt_id) {
    auto it = m_stmt_info.find(stmt_id);
    if (it == m_stmt_info.end()) {
        cerr << "Prog2PDS::get_stmt_info failed on id " << stmt_id << endl;
        exit(-1);
    }
    return it->second;
}

HeadInfo& Prog2PDS::getadd_head_info(head h) {
    auto it = m_head_info.find(h);
    if (it == m_head_info.end()) {
        HeadInfo empty;
        it = m_head_info.insert(make_pair(h, empty)).first;
    }
    return it->second;
}

HeadInfo& Prog2PDS::get_head_info(head h) {
    auto it = m_head_info.find(h);
    if (it == m_head_info.end()) {
        cerr << "Prog2PDS::get_head_info failed on head " << h.first << ", " << h.second << endl;
        exit(-1);
    }
    return it->second;
}

void Prog2PDS::do_statements() {
    int init_stmt_id = init_procedure->get_statement()->get_id();
    add_worklist(init_stmt_id, vexp::NULL_HASH);
    do_worklist_reachability();
}
    
vexp::environment_ptr Prog2PDS::make_global_environment() {
    vexp::environment_ptr e = boost::make_shared<vexp::Environment>();
    for (string const& v : prog->get_global_vars()) {
        e->insert_var(v);
    }
    for (string const& v : prog->get_shared_vars()) {
        e->insert_var(v);
    }
    return e;
}

vexp::environment_ptr Prog2PDS::make_shared_environment() {
    vexp::environment_ptr e = boost::make_shared<vexp::Environment>();
    for (string const& v : prog->get_shared_vars()) {
        e->insert_var(v);
    }
    return e;
}

vexp::environment_ptr Prog2PDS::make_wait_environment() {
    vexp::environment_ptr e = boost::make_shared<vexp::Environment>();
    for (string const& v : prog->get_global_vars()) {
        e->insert_var(v);
    }
    return e;
}


vexp::environment_ptr Prog2PDS::make_procedure_environment(procedure_ptr m) {
    vexp::environment_ptr e = boost::make_shared<vexp::Environment>();
    for (string const& v : prog->get_global_vars()) {
        e->insert_var(v);
    }
    for (string const& v : prog->get_shared_vars()) {
        e->insert_var(v);
    }
    for (string const& v : m->get_arguments()) {
        e->insert_var(v);
    }
    for (string const& v : m->get_local_vars()) {
        e->insert_var(v);
    }
    return e;
}


void Prog2PDS::do_block(prog::StatementBlock const& s,
                        int cont,
                        vexp::environment_ptr env) {
    vector<statement_ptr> const& stmts = s.get_statements();
    int hash = env->get_hash();
    if (stmts.size() > 0) {
        int id = stmts[0]->get_id();
        add_worklist(id, hash);
        add_pass_on_watcher(s.get_id(), hash, id, hash);
    } else if (cont != NO_STMT) {
        add_worklist(cont, hash);
        add_pass_on_watcher(s.get_id(), hash, cont, hash);
    }
}



void Prog2PDS::add_sloppy_rewrite(int stmt_id, 
                                  int env_hash,
                                  string const& act,
                                  int cont, 
                                  int next_hash,
                                  vexp::environment_ptr env,
                                  cexp::counterexp_ptr cc,
                                  std::set<std::pair<std::string,int>>* counter_acts) {
    env->restore_from_hash(env_hash);
    string cur_control = make_control(*env);
    string cur_stack   = make_stack(stmt_id, *env);

    StatementInfo const& info = get_stmt_info(stmt_id);
    bool post_switch = info.can_switch_post;

    if (cont != NO_STMT) {
        env->restore_from_hash(next_hash);
        string next_control = make_control(*env);
        string next_stack   = make_stack(cont, *env);
        add_rewrite(cur_control, cur_stack, act, next_control, next_stack, cc, counter_acts);
        
        if (post_switch && !can_pre_switch(cont)) {
            string switch_control = make_control(*env, true);
            add_rewrite(cur_control, 
                        cur_stack, 
                        act, 
                        switch_control, 
                        next_stack, 
                        cc, 
                        counter_acts);
        }

        add_worklist(cont, next_hash);
        add_pass_on_watcher(stmt_id, env_hash, cont, next_hash);
    } else {
        env->restore_from_hash(next_hash);
        string next_control = make_control(*env);
        global_env->set_from(*env);
        int ret_hash = global_env->get_hash();
        add_pop(cur_control, cur_stack, act, next_control, cc, counter_acts);

        if (post_switch) {
            string switch_control = make_control(*env, true);
            add_pop(cur_control, 
                    cur_stack, 
                    act, 
                    switch_control, 
                    cc, 
                    counter_acts);
        }

        //cout << "line 625 adding return hash " << ret_hash << endl;
        getadd_head_info(make_pair(stmt_id, env_hash)).add_return_hash(ret_hash);
    }
}

void Prog2PDS::do_switch_point_pre(int stmt_id, vexp::environment_ptr env) {
    // nothing to add if already a switch point
    string cur_control = make_control(*env);
    string cur_stack   = make_stack(stmt_id, *env);
    string switch_control = make_control(*env, true);

    cexp::counterexp_ptr null_cc;

    add_rewrite(cur_control, cur_stack, E_ACT, switch_control, cur_stack, null_cc);
    // Let's let global rules take care of switching back to non-switch
    // control
    // add_rewrite(switch_control, cur_stack, E_ACT, cur_control, cur_stack, null_cc);
}


void Prog2PDS::do_cond_jump(bool nondet,
                            vexp::varexpression_ptr vc,
                            cexp::counterexp_ptr cc,
                            int stmt_id,
                            int then_cont,
                            int else_cont,
                            vexp::environment_ptr env) {
    int env_hash = env->get_hash();
    
    // case 1: nondet
    if (nondet) {
        cexp::counterexp_ptr null_cc;
        add_sloppy_rewrite(stmt_id, env_hash, E_ACT, then_cont, env_hash, env, null_cc);
        add_sloppy_rewrite(stmt_id, env_hash, E_ACT, else_cont, env_hash, env, null_cc);
    } else {
        // case 2: no counter condition
        if (!cc && !vc) {
            cerr << "Prog2PDS::do_cond_jump passed null counter and var constraint, but nondet is false." 
                 << endl;
            exit(-1);
        } else if (!cc) {
            // no counter constraint, can determine dir now
            bool vc_val = vc ? vc->evaluate(*env) : true;
            int cont = vc_val ? then_cont : else_cont; 
            add_sloppy_rewrite(stmt_id, env_hash, E_ACT, cont, env_hash, env, cc);
        } else {
            bool vc_val = vc ? vc->evaluate(*env) : true;
            // if value's always false, go to else branch
            if (!vc_val) {
                cexp::counterexp_ptr null_exp;
                string next_stack = make_stack(else_cont, *env);
                add_sloppy_rewrite(stmt_id, env_hash, E_ACT, else_cont, env_hash, env, null_exp);
            } else {
                // else, depends on counter constraint
                cexp::counterexp_ptr not_cc = boost::make_shared<cexp::CExpNot>(cc);
                add_sloppy_rewrite(stmt_id, env_hash, E_ACT, then_cont, env_hash, env, cc);
                add_sloppy_rewrite(stmt_id, env_hash, E_ACT, else_cont, env_hash, env, not_cc);
            }
        }
    }
}


void Prog2PDS::do_if(prog::StatementIf const& s, 
                     int cont, 
                     vexp::environment_ptr env) {
    statement_ptr else_stmt = s.get_else_stmt();
    int else_cont = else_stmt ? else_stmt->get_id() : cont;
    do_cond_jump(s.is_nondet(),
                 s.get_var_conditional(),
                 s.get_counter_conditional(),
                 s.get_id(),
                 s.get_then_stmt()->get_id(),
                 else_cont,
                 env);
}

void Prog2PDS::do_while(prog::StatementWhile const& s, 
                               int cont,
                               vexp::environment_ptr env) {
    do_cond_jump(s.is_nondet(),
                 s.get_var_conditional(),
                 s.get_counter_conditional(),
                 s.get_id(),
                 s.get_body()->get_id(),
                 cont,
                 env);
}





void Prog2PDS::do_assign(prog::StatementAssign const& s, 
                         int cont,
                         vexp::environment_ptr env) {
    cexp::counterexp_ptr null_cc;
    int s_id = s.get_id();

    int old_hash = env->get_hash();

    for (auto assigns : s.get_assigns()) {
        string const& lhs = assigns.first;
        vexp::varexpression_ptr rhs = assigns.second;
        if (rhs)
            env->set_value(lhs, rhs->evaluate(*env));
        else {
            cerr << "Prog2PDS::do_assign found a null rhs in: " << s << endl;
            exit(-1);
        }
    }

    int new_hash = env->get_hash();

    add_sloppy_rewrite(s_id, old_hash, E_ACT, cont, new_hash, env, null_cc);
}


void Prog2PDS::do_call(prog::StatementCall const& s, 
                       int cont,
                       vexp::environment_ptr env) {
    int s_id = s.get_id();
    StatementInfo const& info = get_stmt_info(s_id);
    bool post_switch = info.can_switch_post;

    procedure_ptr m = get_name_procedure(s.get_procedure_name());
    statement_ptr init_s = m->get_statement();
    int init_id = init_s->get_id();
    StatementInfo const& init_info = get_stmt_info(init_id);
    vexp::environment_ptr m_env = init_info.env;

    global_env->set_from(*env);
    m_env->set_from(*global_env);

    vector<vexp::varexpression_ptr> const& args = s.get_arguments();
    vector<string> const& arg_names = m->get_arguments();
    auto it = args.begin();
    for (string const& name : arg_names) {
        bool val = (*it++)->evaluate(*env);
        m_env->set_value(name, val);
    }

    string cur_control  = make_control(*env);
    string cur_stack    = make_stack(s_id, *env);
    string callee_stack = make_stack(init_id, *m_env);

    if (cont != NO_STMT) {
        string ret_stack = make_stack(cont, *env);

        cexp::counterexp_ptr null_cc;
        add_push(cur_control, 
                 cur_stack, 
                 E_ACT, 
                 cur_control, 
                 callee_stack, 
                 ret_stack, 
                 null_cc);

        if (post_switch && !can_pre_switch(cont)) {
            string switch_control = make_control(*env, true);
            add_push(cur_control, 
                     cur_stack, 
                     E_ACT, 
                     switch_control, 
                     callee_stack, 
                     ret_stack, 
                     null_cc);
        }

        int cur_env_hash = env->get_hash();
        int m_env_hash   = m_env->get_hash();
        add_fun_call_watcher(s_id, cur_env_hash, cont, init_id, m_env_hash);
        add_worklist(init_id, m_env_hash);
    } else {
        cexp::counterexp_ptr null_cc;
        add_rewrite(cur_control, cur_stack, E_ACT, cur_control, callee_stack, null_cc);

        if (post_switch) {
            string switch_control = make_control(*env, true);
            add_rewrite(cur_control, 
                        cur_stack, 
                        E_ACT, 
                        switch_control, 
                        callee_stack, 
                        null_cc);
        }

        int cur_env_hash = env->get_hash();
        int m_env_hash   = m_env->get_hash();
        add_pass_on_watcher(s_id, cur_env_hash, init_id, m_env_hash);
        add_worklist(init_id, m_env_hash);
    }
}

void Prog2PDS::do_return(prog::StatementReturn const& s, 
                         vexp::environment_ptr env) {
    cexp::counterexp_ptr null_cc;
    int s_id = s.get_id();
    StatementInfo const& info = get_stmt_info(s_id);

    string cur_control = make_control(*env);
    string cur_stack   = make_stack(s_id, *env);

    add_pop(cur_control, cur_stack, E_ACT, cur_control, null_cc);

    if (info.can_switch_post) {
        string switch_control = make_control(*env, true);
        add_pop(cur_control, cur_stack, E_ACT, switch_control, null_cc);
    }

    int env_hash = env->get_hash();
    global_env->set_from(*env);
    int ret_hash = global_env->get_hash();

//    cout << "line 839 adding return hash " << ret_hash << endl;
//    cout << "from environment " << *global_env << endl;
    getadd_head_info(make_pair(s_id, env_hash)).add_return_hash(ret_hash);
}



void Prog2PDS::do_counter_adj(prog::StatementCounterAdj const& s, 
                              int cont,
                              vexp::environment_ptr env) {
    cexp::counterexp_ptr null_cc;
    int s_id = s.get_id();
    set<pair<string,int>> counter_acts;
    counter_acts.insert(make_pair(s.get_counter(), s.get_increment()));

    int env_hash = env->get_hash();
    add_sloppy_rewrite(s_id, env_hash, E_ACT, cont, env_hash, env, null_cc, &counter_acts);
}


void Prog2PDS::do_echo(prog::StatementEcho const& s, 
                       int cont,
                       vexp::environment_ptr env) {
    cexp::counterexp_ptr null_cc;
    int s_id = s.get_id();
    set<pair<string,int>> counter_acts;

    int env_hash = env->get_hash();
    add_sloppy_rewrite(s_id, 
                       env_hash, 
                       make_action(s.get_action()), 
                       cont, 
                       env_hash, 
                       env, 
                       null_cc, 
                       &counter_acts);
}


void Prog2PDS::do_switch(prog::StatementSwitch const& s,
                         int cont,
                         vexp::environment_ptr env) {
    int env_hash = env->get_hash();
    int stmt_id  = s.get_id();
    
    cexp::counterexp_ptr null_cc;

    for (statement_ptr const& sub_s : s.get_branches()) {
        add_sloppy_rewrite(stmt_id, env_hash, E_ACT, sub_s->get_id(), env_hash, env, null_cc);
    }
}


void Prog2PDS::do_assert(prog::StatementAssert const& s,
                         int cont,
                         vexp::environment_ptr env) {
    int stmt_id  = s.get_id();
    int env_hash = env->get_hash();

    vexp::varexpression_ptr vc = s.get_var_conditional();
    cexp::counterexp_ptr    cc = s.get_counter_conditional();
    
    bool vc_val = vc ? vc->evaluate(*env) : true;
    if (!vc_val) {
        // assert failed, goto err
        cexp::counterexp_ptr null_cc;
        add_goto_control(stmt_id, env_hash, env, make_err_action(), make_final(), null_cc);
    } else {
        if (!cc) { 
            // no counter constraint, so we're on our way
            add_sloppy_rewrite(stmt_id, env_hash, E_ACT, cont, env_hash, env, cc);
        } else {
            // depends on counter constraint
            add_sloppy_rewrite(stmt_id, env_hash, E_ACT, cont, env_hash, env, cc);

            cexp::counterexp_ptr not_cc = boost::make_shared<cexp::CExpNot>(cc);
            add_goto_control(stmt_id, env_hash, env, make_err_action(), make_final(), not_cc);
        }
    }
}


void Prog2PDS::do_lock(prog::StatementLock const& s, 
                       int cont,
                       vexp::environment_ptr env) {
    cexp::counterexp_ptr null_cc;
    int s_id = s.get_id();

    bool wanted_pre_val = !s.get_lock();
    string const& var = s.get_var();

    int old_hash = env->get_hash();

    if (env->get_value(var) == wanted_pre_val) {
        env->set_value(var, !wanted_pre_val);

        int new_hash = env->get_hash();

        add_sloppy_rewrite(s_id, old_hash, E_ACT, cont, new_hash, env, null_cc);
    }
}


void Prog2PDS::do_goto(prog::StatementGoto const& s, 
                       vexp::environment_ptr env) {
    cexp::counterexp_ptr null_cc;
    int s_id = s.get_id();
    int cont = get_label_id(s.get_goto_label());
    int hash = env->get_hash();

    add_sloppy_rewrite(s_id, hash, E_ACT, cont, hash, env, null_cc);
}




void Prog2PDS::add_goto_control(int stmt_id,
                                int env_hash,
                                vexp::environment_ptr env,
                                std::string const& action,
                                std::string const& dest_control,
                                cexp::counterexp_ptr cc) {
    int old_hash = env->get_hash();
    env->restore_from_hash(env_hash);
    string cur_control = make_control(*env);
    string cur_stack   = make_stack(stmt_id, *env);
    add_rewrite(cur_control, cur_stack, action, dest_control, cur_stack, cc);
    env->restore_from_hash(old_hash);
}


void Prog2PDS::add_goto_from_stack(std::string const& from_stack,
                                   vexp::environment_ptr env,
                                   int to_stmt_id,
                                   std::string const& action,
                                   cexp::counterexp_ptr cc) {
    string cur_control = make_control(*env);
    string next_stack  = make_stack(to_stmt_id, *env);
    add_rewrite(cur_control, from_stack, action, cur_control, next_stack, cc);
}


gls Prog2PDS::get_var_scope(std::string const& v) {
    auto it = get_var_scope_memo.find(v);
    gls scope;
    if (it != get_var_scope_memo.end()) {
        scope = it->second;
    } else {
        bool found = false;
        set<string> const& vars = prog->get_global_vars();
        auto it = vars.begin();
        while (!found && (it != vars.end())) {
            found = (*it == v); 
            ++it;
        }

        if (found) {
            scope = GLOBAL;
        } else {
            set<string> const& vars = prog->get_shared_vars();
            auto it = vars.begin();
            while (!found && (it != vars.end())) {
                found = (*it == v); 
                ++it;
            }

            if (found) {
                scope = SHARED;
            } else {
                scope = LOCAL;
            }
        }
        get_var_scope_memo.insert(make_pair(v, scope));
    }

    return scope;
}

std::string Prog2PDS::make_wait_control(vexp::Environment const& e,
                                        std::string const& pds_name) {
    std::ostringstream s;
    s << "w_";
    e.env_iter([&, this] (string const& var, bool val) {
        if (get_var_scope(var) == GLOBAL) {
            s << var << val << "_";
        }
    });
    s << pds_name;
    return s.str();
}


std::string Prog2PDS::make_control(vexp::Environment const& e,
                                   std::string const& pds_name,
                                   bool can_switch) {
    std::ostringstream s;
    s << "q_";
    e.env_iter([&, this] (string const& var, bool val) {
        if (get_var_scope(var) != LOCAL) {
            s << var << val << "_";
        }
    });
    s << pds_name;
    if (can_switch) {
        s << "_" << CAN_SWITCH;
    }
    return s.str();
}


std::string Prog2PDS::make_stack(int stmt_id, vexp::Environment const& e) {
    std::ostringstream s; 
    s << get_name_from_id(stmt_id) << "_";

    e.env_iter([&, this] (string const& var, bool val) {
        if (get_var_scope(var) == LOCAL) {
            s << var << val << "_";
        }
    });

    s << pds_name;

    return s.str();
}

void Prog2PDS::add_rewrite(std::string const& p,
                           std::string const& a,
                           std::string const& act,
                           std::string const& q,
                           std::string const& b,
                           cexp::counterexp_ptr cc,
                           set<pair<string,int>>* counter_acts) {
    vector<string> w;
    w.push_back(b);

    rule_const_ptr r;
    
    if (counter_acts) 
        r = boost::make_shared<Rule>(p, a, act, q, w, cc, *counter_acts);
    else {
        set<pair<string,int>> c_acts;
        r = boost::make_shared<Rule>(p, a, act, q, w, cc, c_acts);
    }

    pds->add_rule(r);
}

void Prog2PDS::add_pop(std::string const& p,
                       std::string const& a,
                       std::string const& act,
                       std::string const& q,
                       cexp::counterexp_ptr cc,
                       set<pair<string,int>>* counter_acts) {
    vector<string> w;

    rule_const_ptr r;

    if (counter_acts) 
        r = boost::make_shared<Rule>(p, a, act, q, w, cc, *counter_acts);
    else {
        set<pair<string,int>> c_acts;
        r = boost::make_shared<Rule>(p, a, act, q, w, cc, c_acts);
    }

    pds->add_rule(r);
}

void Prog2PDS::add_push(std::string const& p,
                        std::string const& a,
                        std::string const& act,
                        std::string const& q,
                        std::string const& b,
                        std::string const& c,
                        cexp::counterexp_ptr cc,
                        set<pair<string,int>>* counter_acts) {
    vector<string> w;
    w.push_back(b);
    w.push_back(c);

    rule_const_ptr r;

    if (counter_acts) 
        r = boost::make_shared<Rule>(p, a, act, q, w, cc, *counter_acts);
    else {
        set<pair<string,int>> c_acts;
        r = boost::make_shared<Rule>(p, a, act, q, w, cc, c_acts);
    }

    pds->add_rule(r);
}


std::string const& Prog2PDS::get_name_from_id(int stmt_id) {
    return get_stmt_info(stmt_id).name;
}



void Prog2PDS::do_worklist_reachability() {
    while (!worklist.empty()) {
        auto head    = worklist.begin();
        int stmt_id  = head->first;
        int env_hash = head->second;
        worklist.erase(head);
        mark_head_reached(stmt_id, env_hash);
        do_head_reachability(stmt_id, env_hash);
    }
}





bool Prog2PDS::head_reached(int stmt_id, int env_hash) {
    return reachedlist.find(make_pair(stmt_id, env_hash)) != reachedlist.end();
}

void Prog2PDS::mark_head_reached(int stmt_id, int env_hash) {
    reachedlist.insert(make_pair(stmt_id, env_hash));
}


void Prog2PDS::do_head_reachability(int stmt_id, int env_hash) {
    class Switcher : StatementVisitor {
        Prog2PDS* daddy;
        StatementInfo* info;

        public:

            Switcher(Prog2PDS* new_dad) {
                daddy = new_dad;
            }

            void switch_on(StatementInfo& si, int env_hash) {
                info = &si;
                si.env->restore_from_hash(env_hash);

                if (si.can_switch_pre) {
                    daddy->do_switch_point_pre(si.ptr->get_id(), 
                                               si.env);
                }

               // cout << "doing statement (id " << si.ptr->get_id() << ") " << *si.ptr << endl;

                si.ptr->accept(*this);

                // cout << "done statement (id " << si.ptr->get_id() << ") " << *si.ptr << endl;
            }

            virtual void visit(StatementBlock& s) { 
                daddy->do_block(s, info->successor, info->env);
            }

            virtual void visit(StatementAssign& s) {
                daddy->do_assign(s, info->successor, info->env);
            }

            virtual void visit(StatementIf& s) { 
                daddy->do_if(s, info->successor, info->env);
            }

            virtual void visit(StatementWhile& s) { 
                daddy->do_while(s, info->successor, info->env);
            }

            virtual void visit(StatementCall& s) { 
                daddy->do_call(s, info->successor, info->env); 
            }

            virtual void visit(StatementReturn& s) {
                daddy->do_return(s, info->env);
            }

            virtual void visit(StatementCounterAdj& s) { 
                daddy->do_counter_adj(s, info->successor, info->env);
            }

            virtual void visit(StatementEcho& s) { 
                daddy->do_echo(s, info->successor, info->env);
            }

            virtual void visit(StatementSwitch& s) {
                daddy->do_switch(s, info->successor, info->env);
            }

            virtual void visit(StatementAssert& s) {
                daddy->do_assert(s, info->successor, info->env);
            }

            virtual void visit(StatementLock& s) {
                daddy->do_lock(s, info->successor, info->env);
            }

            virtual void visit(StatementGoto& s) {
                daddy->do_goto(s, info->env);
            }
    } switcher(this);
    StatementInfo& si = get_stmt_info(stmt_id);
    switcher.switch_on(si, env_hash);
}


void Prog2PDS::pass_on_watcher(int stmt_id, int env_hash, int new_ret_hash) {
    getadd_head_info(make_pair(stmt_id, env_hash)).add_return_hash(new_ret_hash);
}


void Prog2PDS::fun_call_watcher(int stmt_id,  
                                int env_hash, 
                                int ret_to_stmt_id, 
                                int new_ret_hash) {
    // get return to env
    vexp::environment_ptr ret_stmt_env = getadd_stmt_info(ret_to_stmt_id).env;
    global_env->restore_from_hash(new_ret_hash);
    // hacky, uses fact that call and return to loc are in same procedure
    ret_stmt_env->restore_from_hash(env_hash);
    ret_stmt_env->set_from(*global_env);
    int ret_hash = ret_stmt_env->get_hash();
    
    // do rest
    if (!head_reached(ret_to_stmt_id, ret_hash)) {
        add_pass_on_watcher(stmt_id, env_hash, ret_to_stmt_id, ret_hash);
        add_worklist(ret_to_stmt_id, ret_hash);
    }
}


void PassOnWatcher::f(int new_ret_hash) const {
    daddy->pass_on_watcher(stmt_id, env_hash, new_ret_hash);
}


void FunCallWatcher::f(int new_ret_env) const {
    daddy->fun_call_watcher(stmt_id, env_hash, ret_to_stmt_id, new_ret_env);
}



// because we can context switch and change shared vals at all times, we have to
// do for all shared envs
void Prog2PDS::add_pass_on_watcher(int stmt_id, 
                                   int env_hash, 
                                   int w_stmt_id, 
                                   int w_env_hash) {
    watcher_ptr f = boost::make_shared<PassOnWatcher>(this,
                                                      stmt_id,
                                                      env_hash);

    vexp::environment_ptr w_env = getadd_stmt_info(w_stmt_id).env;
    shared_env->forall_vals([&] (vexp::Environment& se) {
        w_env->restore_from_hash(w_env_hash);
        w_env->set_from(se);
        getadd_head_info(make_pair(w_stmt_id, w_env->get_hash())).add_watcher(f);
    });
}



// because we can context switch and change shared vals at all times, we have to
// do for all shared envs
void Prog2PDS::add_fun_call_watcher(int stmt_id,
                                    int env_hash,
                                    int ret_stmt_id,
                                    int w_stmt_id,
                                    int w_env_hash) {
    watcher_ptr f = boost::make_shared<FunCallWatcher>(this,
                                                       stmt_id,
                                                       env_hash,
                                                       ret_stmt_id);
    vexp::environment_ptr w_env = getadd_stmt_info(w_stmt_id).env;
    shared_env->forall_vals([&] (vexp::Environment& se) {
        w_env->restore_from_hash(w_env_hash);
        w_env->set_from(se);
        getadd_head_info(make_pair(w_stmt_id, w_env->get_hash())).add_watcher(f);
    });
}                                    


// because we can context switch and change shared vals at all times, we have to
// do for all shared envs
void Prog2PDS::add_worklist(int stmt_id, int env_hash) {
    vexp::environment_ptr env = getadd_stmt_info(stmt_id).env;
    shared_env->forall_vals([&] (vexp::Environment& se) {
        env->restore_from_hash(env_hash);
        env->set_from(se);
        int new_hash = env->get_hash();
        if (!head_reached(stmt_id, new_hash)) {
            worklist.insert(make_pair(stmt_id, new_hash));    
        }
    });
}


// globals are essentially the following:
//
// (active_env, wait_env, wait_env...) -> (wait_env, wait_env, active_env...)
//
// i.e., the active thread goes to wait, and any other wakes up
//
// We also have transitions where all threads that should be waiting are really
// taken to their final state -- this accounts for the final context switch
// where we want everything to be final
//
// then instead of active_env we may also have the final for that thread...
// (because threads may finish...) (we use a negative env to represent this)


void Prog2PDS::add_global_transitions() {
    vector<int> env_hashes;
    do_globals(env_hashes, -1, 0);
}

// note env_hashes by ref so when we change it we have to unchange it
void Prog2PDS::do_globals(vector<int>& env_hashes, int change_idx, int cur_idx) {
    int n = prog->get_init_procedures().size();
    if (cur_idx >= n) {
        do_globals_add_phase(env_hashes, change_idx);
    } else {
        wait_env->forall_vals([&] (vexp::Environment& e) {
            env_hashes.push_back(e.get_hash());
            do_globals(env_hashes, change_idx, cur_idx + 1);
            // unchange it
            env_hashes.pop_back();
        });
        // when it's terminated
        env_hashes.push_back(-1);
        do_globals(env_hashes, change_idx, cur_idx + 1);
        env_hashes.pop_back();

        // when it's the scheduled thread
        if (change_idx == -1) {
            // when it's active
            global_env->forall_vals([&] (vexp::Environment& e) {
                env_hashes.push_back(e.get_hash());
                do_globals(env_hashes, cur_idx, cur_idx + 1);
                env_hashes.pop_back();
            });
            // when it's terminated
            env_hashes.push_back(-1);
            do_globals(env_hashes, cur_idx, cur_idx + 1);
            env_hashes.pop_back();
        }
    }
}



void Prog2PDS::do_globals_add_phase(vector<int>& env_hashes, int change_idx) {
    // not if no change, or changing a thread thread that's already ended
    if (change_idx != -1) {
        int n = prog->get_init_procedures().size();

        // we only add a rule if each control state is a member of the pdss
        std::vector<pds::pds_ptr> const& pdss = mpds->get_pdss();


        vector<string> before_controls;
        bool is_end = true;
        bool is_valid = true;

        for (int i = 0; is_valid && i < n; i++) {
            if (env_hashes[i] == -1) {
                string const& control = make_final(make_thread_name(i));
                if (pdss[i]->has_control(control)) 
                    before_controls.push_back(control);
                else
                    is_valid = false;
            } else if (i == change_idx) {
                is_end = false;
                global_env->restore_from_hash(env_hashes[i]);
                // true as arg because we only take can_switch controls
                string const& control = make_control(*global_env, 
                                                     make_thread_name(i),
                                                     true);
                if (pdss[i]->has_control(control))
                    before_controls.push_back(control);
                else
                    is_valid = false;
            } else {
                is_end = false;
                wait_env->restore_from_hash(env_hashes[i]);
                string const& control = make_wait_control(*wait_env, make_thread_name(i));
                if (pdss[i]->has_control(control))
                    before_controls.push_back(control);
                else
                    is_valid = false;
            }
        }

        if (!is_end && is_valid && env_hashes[change_idx] != -1) {
            // note global env will contain value of shared vars from active thread
            // (change_idx)
            
            // these rules are for when we expect more context switches
            int cur_alive_hash = global_env->get_hash();

            for (int new_active = 0; new_active < n; new_active++) {
                // don't change to same thread or an ended one
                if (new_active != change_idx &&
                    env_hashes[new_active] != -1) {
                    vector<string> after_controls;
                    is_valid = true;
                    for (int i = 0; is_valid && i < n; i++) {
                        if (env_hashes[i] == -1) {
                            // stay terminated
                            string const& control = make_final(make_thread_name(i));
                            if (pdss[i]->has_control(control))
                                after_controls.push_back(control);
                            else
                                is_valid = false;
                        } else if (new_active == i) {
                            wait_env->restore_from_hash(env_hashes[i]);
                            // overwrite all saved vals
                            // (keeping val of all shared vals)
                            global_env->set_from(*wait_env);
                            string const& control = make_control(*global_env, 
                                                                  make_thread_name(i));
                            if (pdss[i]->has_control(control)) 
                                after_controls.push_back(control);
                            else
                                is_valid = false;
                            // restore
                            global_env->restore_from_hash(cur_alive_hash);
                        } else {
                            if (i == change_idx) {
                                // note: still leaves global hash at val of active
                                // thread
                                global_env->restore_from_hash(env_hashes[i]);
                                string const& control = make_wait_control(*global_env,
                                                                           make_thread_name(i));
                                if (pdss[i]->has_control(control))
                                    after_controls.push_back(control);
                                else
                                    is_valid = false;
                            } else {
                                wait_env->restore_from_hash(env_hashes[i]);
                                string const& control = make_wait_control(*wait_env, 
                                                                           make_thread_name(i));
                                if (pdss[i]->has_control(control))
                                    after_controls.push_back(control);
                                else
                                    is_valid = false;
                            }
                        }
                    }
                    if (is_valid) {
                        globalrule_ptr r = boost::make_shared<GlobalRule>(before_controls, 
                                                                          after_controls);
                        mpds->add_global_rule(r);
                    }
                }
            }

            // these rules are for when we expect no more context switches
            for (int new_active = 0; new_active < n; new_active++) {
                // don't change to same thread or an ended one
                if (new_active != change_idx &&
                    env_hashes[new_active] != -1) {
                    vector<string> after_controls;
                    is_valid = true;
                    for (int i = 0; is_valid && i < n; i++) {
                        if (new_active == i &&
                            env_hashes[i] != -1) {
                            wait_env->restore_from_hash(env_hashes[i]);
                            // overwrite all saved vals
                            // (keeping val of all shared vals)
                            global_env->set_from(*wait_env);
                            string const& control = make_control(*global_env, 
                                                                  make_thread_name(i));
                            if (pdss[i]->has_control(control)) 
                                after_controls.push_back(control);
                            else
                                is_valid = false;
                            // restore
                            global_env->restore_from_hash(cur_alive_hash);
                        } else {
                            string const& control = make_final(make_thread_name(i));
                            if (pdss[i]->has_control(control))
                                after_controls.push_back(control);
                            else
                                is_valid = false;
                        }
                    }
                    if (is_valid) {
                        globalrule_ptr r = boost::make_shared<GlobalRule>(before_controls, 
                                                                          after_controls);
                        mpds->add_global_rule(r);
                    }
                }
            }
        // if the alive thread just ended, allow everyone else to too
        } else if (!is_end && is_valid && env_hashes[change_idx] == -1) {
            vector<string> after_controls;
            is_valid = true;
            for (int i = 0; is_valid && i < n; i++) {
                string const& control = make_final(make_thread_name(i));
                if (pdss[i]->has_control(control))
                    after_controls.push_back(control);
                else
                    is_valid = false;
            }
            if (is_valid) {
                globalrule_ptr r = boost::make_shared<GlobalRule>(before_controls, 
                                                                  after_controls);
                mpds->add_global_rule(r);
            }
        }
    }
}


// essentially replace all instances of a var in constraint with sum of the
// threads and build the following:
//
//     error > 0 || (done = num_threads && user_constraint)
//
// that is, we reach an error (assertion failed) or the run finishes normally
// but is an error as specified by the user (e.g. frees != allocs)
void Prog2PDS::add_constraint() {
    class Builder : public pres::EPresVisitor,
                    public pres::EPresValVisitor {
  
        Prog2PDS* daddy;
        multipds_ptr mpds;
        pres::epresburger_ptr ret_ptr;
        pres::epres_val_ptr   ret_ptr_val;

        public:
            Builder(Prog2PDS* new_daddy, multipds_ptr the_mpds) :
                daddy(new_daddy),
                mpds(the_mpds) { }

            pres::epresburger_ptr build(pres::epresburger_ptr init) {
                ret_ptr.reset();
                if (init)
                    init->accept(*this);
                return ret_ptr;
            }

            pres::epres_val_ptr build(pres::epres_val_ptr init) {
                ret_ptr_val.reset();
                if (init)
                    init->accept(*this);
                return ret_ptr_val;
            }

            virtual void visit(pres::EPresCompare& obj) {
                pres::epres_val_ptr p1 = build(obj.get_lhs());
                pres::epres_val_ptr p2 = build(obj.get_rhs());
                ret_ptr = boost::make_shared<pres::EPresCompare>(p1, obj.get_op(), p2);
            }


            virtual void visit(pres::EPresImplies& obj) {
                pres::epresburger_ptr p1 = build(obj.get_lhs());
                pres::epresburger_ptr p2 = build(obj.get_rhs());
                ret_ptr = boost::make_shared<pres::EPresImplies>(p1, p2);
            }

            virtual void visit(pres::EPresAnd& obj) {
                pres::epres_and_ptr r = boost::make_shared<pres::EPresAnd>();
                for (pres::epresburger_ptr e : obj.get_operands()) {
                    pres::epresburger_ptr p = build(e);
                    r->add_operand(p);
                }
                ret_ptr = r;
            }

            virtual void visit(pres::EPresOr& obj) {
                pres::epres_or_ptr r = boost::make_shared<pres::EPresOr>();
                for (pres::epresburger_ptr e : obj.get_operands()) {
                    pres::epresburger_ptr p = build(e);
                    r->add_operand(p);
                }
                ret_ptr = r;
            }

            virtual void visit(pres::EPresNot& obj) {
                pres::epresburger_ptr p = build(obj.get_expression());
                ret_ptr = boost::make_shared<pres::EPresNot>(p);
            }

            // note: assumes we don't e quantify a named action
            virtual void visit(pres::EPresExists& obj) {
                pres::epresburger_ptr p = build(obj.get_fmla());
                ret_ptr = boost::make_shared<pres::EPresExists>(obj.get_vars(), p);
            }

            virtual void visit(pres::EPresConst& obj) {
                ret_ptr = boost::make_shared<pres::EPresConst>(obj.get_value());
            }

            virtual void visit(pres::EPresVar& obj) {
                ret_ptr_val = get_var_fmla(obj.get_name());
            }

            virtual void visit(pres::EPresPlus& obj) {
                pres::epres_plus_ptr ret = boost::make_shared<pres::EPresPlus>();
                for (pres::epres_val_ptr v : obj.get_operands()) {
                    pres::epres_val_ptr p = build(v);
                    ret->add_operand(p);
                }
                ret_ptr_val = ret;
            }

            virtual void visit(pres::EPresConstMult& obj) {
                pres::epres_val_ptr p = build(obj.get_scalee());
                ret_ptr_val = boost::make_shared<pres::EPresConstMult>(obj.get_scalar(), p);
            }

            virtual void visit(pres::EPresInteger& obj) {
                ret_ptr_val = boost::make_shared<pres::EPresInteger>(obj.get_value());
            }

            virtual void visit(pres::EPresMinus& obj) {
                pres::epres_val_ptr p1 = build(obj.get_lhs());
                pres::epres_val_ptr p2 = build(obj.get_rhs());
                ret_ptr_val = boost::make_shared<pres::EPresMinus>(p1, p2);
            }

        private:
            map<string, pres::epres_val_ptr> var_fmlas;
            pres::epres_val_ptr get_var_fmla(string const& v) {
                auto vf = var_fmlas.find(v);
                if (vf != var_fmlas.end())
                    return (*vf).second;
                else {
                    pres::epres_val_ptr ret;
                    vector<pres::epres_val_ptr> summands;
                    int n = 0;
                    for (pds_ptr pds : mpds->get_pdss()) {
                        // if the thread will ever be run
                        if (n == 0 || mpds->get_context_switches() > 0) {
                            string vname = daddy->make_action(v, daddy->make_thread_name(n));
                            set<string> const& acts = pds->get_actions();
                            if (acts.find(vname) != acts.end()) {
                                pres::epres_var_ptr vptr = boost::make_shared<pres::EPresVar>(vname);
                                summands.push_back(vptr);
                            }
                        }
                        n++;
                    }

                    if (summands.size() == 0) {
                        ret = boost::make_shared<pres::EPresInteger>(0);
                    } else if (summands.size() == 1) {
                        ret = summands[0];
                    } else {
                        ret = boost::make_shared<pres::EPresPlus>(summands);
                    }
                    var_fmlas.insert(make_pair(v, ret));
                    return ret;
                }
            }


    } builder(this, mpds);

    pres::epresburger_ptr user  = prog->get_constraint();
    pres::epres_val_ptr   error = boost::make_shared<pres::EPresVar>(ERR_ACTION);
    pres::epres_val_ptr   done  = boost::make_shared<pres::EPresVar>(DONE_ACTION);
    pres::epres_val_ptr   zero  = boost::make_shared<pres::EPresInteger>(0);
    pres::epres_val_ptr   n     = boost::make_shared<pres::EPresInteger>
                                      (prog->get_init_procedures().size());

    pres::epresburger_ptr is_error = boost::make_shared<pres::EPresCompare>
                                         (zero, pres::LT, error);
    pres::epresburger_ptr all_done = boost::make_shared<pres::EPresCompare>
                                         (done, pres::EQ, n);

    if (user) {
        pres::epresburger_ptr done_user_err = boost::make_shared<pres::EPresAnd>
                                                  (all_done, user);
        pres::epresburger_ptr constraint = boost::make_shared<pres::EPresOr>
                                               (is_error, done_user_err);
        mpds->set_constraint(builder.build(constraint));
    } else {
        mpds->set_constraint(builder.build(is_error));
    }
}



bool Prog2PDS::is_shared(vexp::varexpression_ptr vc) {
    class Trawler : public vexp::VExpVisitor {
        Prog2PDS& daddy;
        bool result;


        public:
            Trawler(Prog2PDS& new_daddy) 
                : daddy(new_daddy), 
                  result(false) { }

            bool is_shared(vexp::varexpression_ptr vc) {
                vc->accept(*this);
                return result;
            }

            virtual void visit(vexp::VExpConst& b) {
                // do nothing
            }

            virtual void visit(vexp::VExpAnd& b) {
                auto it = b.get_operands().begin();
                auto itend = b.get_operands().end();

                while (!result && it != itend) {
                    is_shared(*it);
                    ++it;
                }
            }

            virtual void visit(vexp::VExpOr& b) {
                auto it = b.get_operands().begin();
                auto itend = b.get_operands().end();

                while (!result && it != itend) {
                    is_shared(*it);
                    ++it;
                }
            }

            virtual void visit(vexp::VExpImplies& b) {
                is_shared(b.get_lhs());
                if (!result)
                    is_shared(b.get_rhs());
            }

            virtual void visit(vexp::VExpNot& b) {
                is_shared(b.get_expression());
            }

            virtual void visit(vexp::VExpVar& b) {
                result = daddy.is_shared(b.get_name());
            }
    } trawler(*this);

    bool result = false;
    if (vc) 
        result = trawler.is_shared(vc);
    return result;
}




bool Prog2PDS::can_pre_switch(int stmt_id) {
    StatementInfo& info = get_stmt_info(stmt_id);
    return info.can_switch_pre;
}


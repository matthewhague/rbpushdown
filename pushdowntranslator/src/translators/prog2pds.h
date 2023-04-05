

// NOTE: At the bottom of the file, i explain the algorithm.  It's a messy
// combination of translation and reachability.  Sorry guys...

#pragma once
#ifndef __TRANSLATORS_H__
#define __TRANSLATORS_H__

#include <map>
#include <set>
#include <utility>

#include "../structures/program.h"
#include "../structures/pds.h"
#include "../structures/variableexpression.h"
#include "../structures/multi_pds.h"

namespace prog2pds {

    const std::string FINAL_CONTROL = "qEnd_";
    const std::string ERR_ACTION = "error";
    const std::string DONE_ACTION = "done";
    const std::string STACK_BOT_INIT = "sbot_init_";
    const std::string STACK_BOT_DONE = "sbot_done_";
    const std::string STACK_BOT_END  = "sbot_end_";
    const std::string THREAD_PREFIX = "pd";
    const std::string CAN_SWITCH = "cs";


    class Prog2PDS;

    class WatcherFun;

    typedef boost::shared_ptr<WatcherFun> watcher_ptr;

    class WatcherFun {
        public:
            virtual void f(int new_ret_hash) const = 0;
    };

    class PassOnWatcher : public WatcherFun {
        Prog2PDS* daddy;
        int stmt_id;
        int env_hash;

        public:
            PassOnWatcher(Prog2PDS* new_dad, int new_s_id, int new_e_hash) {
                daddy    = new_dad;
                stmt_id  = new_s_id;
                env_hash = new_e_hash;
            }

            virtual void f(int new_ret_hash) const;
    };


    class FunCallWatcher : public WatcherFun {
        Prog2PDS* daddy;
        int stmt_id;
        int env_hash;
        int ret_to_stmt_id;

        public:

            FunCallWatcher(Prog2PDS* new_dad, int new_s_id, int new_e_hash, int new_rs) {
                daddy          = new_dad;
                stmt_id        = new_s_id;
                env_hash       = new_e_hash;
                ret_to_stmt_id = new_rs;
            }

            virtual void f(int new_ret_env) const;
    };



    class StatementInfo {
        public:
            prog::statement_ptr   ptr;
            std::string           name;
            vexp::environment_ptr env;

            // if a statement depends on a global variable, it should allow a
            // context switch beforehand
            // the pre-switch is dealt with in do_head_reachability, since it
            // can just do a rewrite rule to (current) control with switch=true
            // the post-switch needs to be handled by each function do_assert,
            // do_if, blah blah, because it has to know the control state after
            // the rule.  Most of this is done in add_sloppy_rewrite
            bool can_switch_pre;
            bool can_switch_post;

            // all those statements that don't know their successors
            // wait, it'll need some explaining
            // during rule generation we need to know the successor of every
            // statement.  i.e. we execute line x then go to line x+1, but an, e.g.,
            // assign statement doesn't know it's successor, so we analyse this and
            // store the information here
            // except for if and while statements.  An "if", with a then and else
            // branch has two successors held by the StatementIf class.  With a null
            // else branch, we have to store an entry in m_blk_succs as we move to
            // the code after the if stmt.  Similar for "while".
            int successor;

            StatementInfo() {
                successor = prog::NO_STMT;
            }
    };

    // a head is stmt_id, env_hash
    typedef std::pair<int, int> head;

    class HeadInfo {
        public:
            std::set<int>            return_hashes;
            std::vector<watcher_ptr> watchers;

            void add_return_hash(int hash) {
                bool is_new = return_hashes.insert(hash).second;
                if (is_new) {
//                    std::cout << "add return hash " << hash << " has " << watchers.size() << " watchers." << std::endl;
                    for (watcher_ptr f : watchers) {
                        f->f(hash);
                    }
                }
            }

            void add_watcher(watcher_ptr f) {
                watchers.push_back(f);
                for (int hash : return_hashes) {
                    f->f(hash);
                }
            }
    };

    enum gls { GLOBAL, LOCAL, SHARED };

    class Prog2PDS {
        std::map<int, StatementInfo> m_stmt_info;
        std::map<std::string, int> m_label_ids;
        std::map<std::string, prog::procedure_ptr> m_name_procedure;

        vexp::environment_ptr global_env;
        vexp::environment_ptr shared_env;
        vexp::environment_ptr wait_env;

        prog::program_ptr prog;
        prog::procedure_ptr  init_procedure;
        pds::pds_ptr      pds;
        pds::multipds_ptr mpds;

        std::string pds_name;


        // maps for the algorithm
        std::map<head, HeadInfo> m_head_info;

        // finally, worklist management for worklist translation algorithm
        std::set<head> worklist;
        std::set<head> reachedlist;


        // efficiency memos
        std::map<std::string, gls> get_var_scope_memo;

        public:
            pds::multipds_ptr translate(prog::program_ptr prog);

            // for namer call back in name_statements
            void name_statement(prog::Statement const& s, std::string const& name);

            void do_block(prog::StatementBlock const& s,
                          int cont,
                          vexp::environment_ptr env);
            void do_if(prog::StatementIf const& s,
                       int cont,
                       vexp::environment_ptr env);
            void do_while(prog::StatementWhile const& s,
                          int cont,
                          vexp::environment_ptr env);
            void do_assign(prog::StatementAssign const& s,
                           int cont,
                           vexp::environment_ptr env);
            void do_call(prog::StatementCall const& s,
                         int cont,
                         vexp::environment_ptr env);
            void do_return(prog::StatementReturn const& s,
                           vexp::environment_ptr env);
            void do_counter_adj(prog::StatementCounterAdj const& s,
                                int cont,
                                vexp::environment_ptr env);
            void do_echo(prog::StatementEcho const& s,
                         int cont,
                         vexp::environment_ptr env);
            void do_switch(prog::StatementSwitch const& s,
                           int cont,
                           vexp::environment_ptr env);
            void do_assert(prog::StatementAssert const& s,
                           int cont,
                           vexp::environment_ptr env);
            void do_lock(prog::StatementLock const& s,
                         int cont,
                         vexp::environment_ptr env);
            void do_goto(prog::StatementGoto const& s,
                         vexp::environment_ptr env);

            void register_stmt_ptr(prog::statement_ptr s) {
                if (s) {
                    StatementInfo& info = getadd_stmt_info(s->get_id());
                    info.ptr = s;
                }
            }

            void register_successor(int id1, int id2) {
                StatementInfo& info1 = getadd_stmt_info(id1);
                if (info1.successor == prog::NO_STMT)
                    info1.successor = id2;
                else {
                    std::cerr << "Stmt " << id1 << " has multi-successors"
                              << std::endl;
                    exit(-1);
                }
            }



            void pass_on_watcher(int stmt_id, int env_hash, int new_ret_hash);
            void fun_call_watcher(int stmt_id,
                                  int env_hash,
                                  int ret_to_stmt_id,
                                  int new_ret_hash);

            // needed by builder in add_constraint()
            std::string make_thread_name(int n);
            std::string make_action(std::string const& act) {
                return make_action(act, pds_name);
            }
            std::string make_action(std::string const& act,
                                    std::string const& pds_name) {
                return act + "_" + pds_name;
            }


        private:
            void reset_translator();
            void add_init_and_fin(bool init_scheduled);
            void build_maps();
            void do_statements();

            pds::pds_ptr translate_thread(prog::procedure_ptr init_procedure,
                                          std::string thread_name,
                                          bool init_scheduled);

            void add_name_procedure(prog::procedure_ptr m) {
                m_name_procedure.insert(std::make_pair(m->get_name(), m));
            }

            prog::procedure_ptr get_name_procedure(std::string const& name) {
                auto it = m_name_procedure.find(name);
                if (it == m_name_procedure.end()) {
                    std::cerr << "Prog2PDS::get_name_procedure of "
                              << name
                              << " failed." << std::endl;
                    exit(-1);
                }
                return (*it).second;
            }

            void set_statement_env(prog::Statement const& s, vexp::environment_ptr e) {
                StatementInfo& info = getadd_stmt_info(s.get_id());
                info.env = e;
            }

            void set_statement_can_switch(prog::Statement const& s,
                                          bool can_switch_pre,
                                          bool can_switch_post) {
                StatementInfo& info = getadd_stmt_info(s.get_id());
                info.can_switch_pre = can_switch_pre;
                info.can_switch_post = can_switch_post;
            }

            vexp::environment_ptr make_procedure_environment(prog::procedure_ptr m);
            vexp::environment_ptr make_global_environment();
            vexp::environment_ptr make_shared_environment();
            vexp::environment_ptr make_wait_environment();

            StatementInfo& getadd_stmt_info(int stmt_id);
            StatementInfo& get_stmt_info(int stmt_id);

            HeadInfo& getadd_head_info(head h);
            HeadInfo& get_head_info(head h);


            std::string make_wait_control(vexp::Environment const& e,
                                          std::string const& pds_name);
            std::string make_control(vexp::Environment const& e,
                                     std::string const& pds_name,
                                     bool can_switch = false);
            std::string make_final(std::string const& pds_name) {
                return FINAL_CONTROL + pds_name;
            }
            std::string make_wait_control(vexp::Environment const& e) {
                return make_wait_control(e, pds_name);
            }
            std::string make_control(vexp::Environment const& e,
                                     bool can_switch = false) {
                return make_control(e, pds_name, can_switch);
            }
            std::string make_stack(int stmt_id, vexp::Environment const& e);
            std::string make_final() { return make_final(pds_name); }
            std::string make_err_action() { return make_action(ERR_ACTION); }
            std::string make_done_action() { return make_action(DONE_ACTION); }
            std::string make_stack_bot_init() { return STACK_BOT_INIT + pds_name; }
            std::string make_stack_bot_done() { return STACK_BOT_DONE + pds_name; }
            std::string make_stack_bot_end() { return STACK_BOT_END + pds_name; }

            void add_rewrite(std::string const& p,
                             std::string const& a,
                             std::string const& act,
                             std::string const& q,
                             std::string const& b,
                             ctrexp::counterexp_ptr cc,
                             std::set<std::pair<std::string,int>>* counter_acts = NULL);

            void add_push(std::string const& p,
                          std::string const& a,
                          std::string const& act,
                          std::string const& q,
                          std::string const& b,
                          std::string const& c,
                          ctrexp::counterexp_ptr cc,
                          std::set<std::pair<std::string,int>>* counter_acts = NULL);

            void add_pop(std::string const& p,
                         std::string const& a,
                         std::string const& act,
                         std::string const& q,
                         ctrexp::counterexp_ptr cc,
                         std::set<std::pair<std::string,int>>* counter_acts = NULL);


            // deals with all the environment and watcher and worklist stuff
            // and adds a pop or rewrite depending on whether cont is NO_STMT
            void add_sloppy_rewrite(int stmt_id,
                                    int env_hash,
                                    std::string const& act,
                                    int cont,
                                    int next_hash,
                                    vexp::environment_ptr env,
                                    ctrexp::counterexp_ptr cc,
                                    std::set<std::pair<std::string,int>>* counter_acts = NULL);

            void add_goto_control(int stmt_id,
                                  int env_hash,
                                  vexp::environment_ptr env,
                                  std::string const& action,
                                  std::string const& dest_control,
                                  ctrexp::counterexp_ptr cc);

            void add_goto_from_stack(std::string const& from_stack,
                                     vexp::environment_ptr env,
                                     int to_stmt_id,
                                     std::string const& action,
                                     ctrexp::counterexp_ptr cc);


            gls get_var_scope(std::string const& v);

            std::string const& get_name_from_id(int stmt_id);

            void do_cond_jump(bool nondet,
                              vexp::varexpression_ptr vc,
                              ctrexp::counterexp_ptr cc,
                              int stmt_id,
                              int then_cont,
                              int else_cont,
                              vexp::environment_ptr env);

            void do_switch_point_pre(int stmt_id, vexp::environment_ptr env);

            void do_worklist_reachability();
            void add_worklist(int stmt_id, int env_hash);
            bool head_reached(int stmt_id, int env_hash);
            void mark_head_reached(int stmt_id, int env_hash);
            void do_head_reachability(int stmt_id, int env_hash);

            void add_pass_on_watcher(int stmt_id,
                                     int env_hash,
                                     int w_stmt_id,
                                     int w_env_hash);

            void add_fun_call_watcher(int stmt_id,
                                      int env_hash,
                                      int ret_stmt_id,
                                      int w_stmt_id,
                                      int w_env_hash);

            void add_global_transitions();
            void do_globals(std::vector<int>& env_hashes, int change_idx, int cur_idx);
            void do_globals_add_phase(std::vector<int>& env_hashes, int change_idx);
            void add_constraint();
            void add_wait_controls(pds::pds_ptr p, std::string const& name);

            bool is_shared(ctrexp::counterexp_ptr cc) {
                // every counter is global, assume at least one is used...
                if (cc) // to get around some bug or other, do this clunky thing
                    return 1;
                else
                    return 0;
            }

            bool is_shared(vexp::varexpression_ptr vc);

            bool is_shared(std::string const& var) {
                return get_var_scope(var) == SHARED;
            }

            bool can_pre_switch(int stmt_id);

            void register_labels(prog::Statement const& s);
            int get_label_id(std::string const& label);
            void add_counters_to_pds();

    };

}

#endif



// ALGORITHM EXPLANATION
//
// So we only want to produce rules that are at least superficially reachable.
// So essentially, if we add p a -> p b, then p b is reachable and we should add
// rules for p b.
//
// This gets tricky when we have p a -> q b c since we don't know what the
// control state will be when we return from b.  So, we attach HeadInfo to each
// head (p a), which contains the set of control states we may return to.  So,
// if we add a rule p a -> q b c, everytime we discover that the head (q b) may
// return to control r, then we need to add rules for the head (r c).
//
// Also, if we have p a -> q b and we discover (q b) may return to r, then (p a)
// may return to r.
//
// So we maintain these return to sets, and a list of "watchers" who are
// interested in when we add something to the return to sets.  There are "pass
// on watchers" which correspond to the second case (i.e. if you can reach it, i
// can too) and "fun call watchers" who are the p a -> q b c rules.
//
// And in all of this, we don't have heads in terms of control states, but a
// statement id and an environment (hash).  From this we can build the control
// and the stack char for the head.
//
// So, in functional pseudo-code, we have:
//
// put init head in worklist
// do_worklist()
//
// do_worklist()
//    while items in work list
//        take one (s, env)
//        do(s, env)
//
//
// do(s, env)
//     translate statement
//     switch statement type
//     pop (p a -> q):
//         add_to_summary(p, a, q)
//
//     rew (p a -> q b ):
//         add_to_worklist(q, b)
//         add_pass_on_watcher(p, a, q, b)
//
//     push (p a -> q b c):
//         add_to_worklist(q, b)
//         add_fun_call_watcher(p, a, q, b)
//
//
// add_to_summary(p, a, q)
//     if q not in (p, a) summary
//         add it
//         alert all watchers q has been added
//
//
//  add_*_watcher(p, a, q, b)
//      add watcher (p, a) to list for (q, b)
//      for all q' in summary of (q, b) alert watcher
//      (to get them up to speed, of course)
//
//
// The pass on watcher is simple:
//
// On alert of new q for watcher (p, a)
//     add_to_summary(p, a, q)
//
//
// The fun call watcher does the following
//
// On alert of new q' for watcher (p, a)
//     Recall we have p a -> q b c
//     add_to_worklist(q', c)
//     add_pass_on_watcher(p, a, q', c)
//
// That is, add rules for q' c, and any returns from that are also returns from
// p a.
//
// Finally, add_to_worklist only adds to the worklist if the pair hasn't been
// done already

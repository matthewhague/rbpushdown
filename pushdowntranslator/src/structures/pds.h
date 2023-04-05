
#pragma once
#ifndef __PDS_H__
#define __PDS_H__

#include <set>
#include <string>
#include <vector>
#include <iostream>
#include <utility>
#include <map>

#include "boost/shared_ptr.hpp"

#include "counterexpression.h"

std::string const E_ACT("z");

namespace pds {

    typedef std::pair<std::string, int> cact;

    class Rule {
        static int next_id;
        int id; 
        std::string p;
        std::string a;
        std::vector<std::string> actions;
        std::string q;
        std::vector<std::string> w;
        cexp::counterexp_ptr guard;
        std::set<cact> counter_acts;

        public:
            Rule(std::string const& p, 
                 std::string const& a,
                 std::string const& action,
                 std::string const& q, 
                 std::vector<std::string> const& w,
                 cexp::counterexp_ptr const& guard, 
                 std::set<cact> const& counter_acts);
            Rule(std::string const& p, 
                 std::string const& a,
                 std::vector<std::string> const& actions,
                 std::string const& q, 
                 std::vector<std::string> const& w,
                 cexp::counterexp_ptr const& guard, 
                 std::set<cact> const& counter_acts);
            ~Rule();

            int get_id() const { return id; }
            std::string const& get_p() const { return p; }
            std::string const& get_a() const { return a; }
            std::string const& get_q() const { return q; }
            std::vector<std::string> const& get_w() const { return w; }
            std::vector<std::string> const& get_actions() const { return actions; }
            std::set<cact> const& get_counter_acts() const { return counter_acts; }
            cexp::counterexp_ptr get_guard() const { return guard; }

            bool operator<(Rule const& rhs) const { return id < rhs.get_id(); }
            friend std::ostream& operator<<(std::ostream& output,  Rule const& r);
    };


    typedef boost::shared_ptr<Rule const> rule_const_ptr;
    bool operator<(rule_const_ptr lhs, rule_const_ptr rhs);

    class Pds;

    typedef boost::shared_ptr<Pds> pds_ptr;
    typedef boost::shared_ptr<Pds const> pds_const_ptr;

    typedef std::pair<std::string, std::string> head;

    std::set<rule_const_ptr> const empty_rule_set;

    class Pds {
        std::set<rule_const_ptr> rules;
        std::set<std::string> controls;
        std::set<std::string> alphabet;
        std::set<std::string> actions;
        std::set<std::string> counters;
        std::set<std::string> freevariables;
        std::map<std::string,int> counter_revs;

        std::string init_p;
        std::string init_a;
        std::string fin_p;

        std::map<head, std::set<rule_const_ptr>> rule_lookup;
        std::map<head, std::set<rule_const_ptr>> rule_rev_lookup;
       
        public:
            Pds();
            Pds(std::set<rule_const_ptr> const& rules);
            ~Pds();

            bool add_rule(rule_const_ptr r);
            void del_rule(rule_const_ptr r);
            void add_rules(std::set<rule_const_ptr> const& add_rules);
            void del_rules(std::set<rule_const_ptr> const& del_rules);

            void add_control(std::string const& s) {
                controls.insert(s);
            }

            void add_alphabet(std::string const& s) {
                alphabet.insert(s);
            }

            void set_init_p(std::string const& s) { init_p = s; }
            void set_init_a(std::string const& s) { init_a = s; }
            void set_fin_p(std::string const& s) { fin_p = s; }

            std::set<rule_const_ptr> const& get_rules() const { return rules; }
            std::set<rule_const_ptr> const& get_rules(std::string const& p,
                                                      std::string const& a) const;
            std::set<rule_const_ptr> const& get_rev_rules(std::string const& p,
                                                          std::string const& a) const;
            std::set<std::string> const& get_controls() const { return controls; }
            std::set<std::string> const& get_alphabet() const { return alphabet; }
            std::set<std::string> const& get_counters() const { return counters; }
            std::set<std::string> const& get_freevariables() const { 
                return freevariables; 
            }
            std::set<std::string> const& get_actions() const { return actions; }
            std::string const& get_init_p() { return init_p; }
            std::string const& get_init_a() { return init_a; }
            std::string const& get_fin_p() { return fin_p; }

            void set_reversals(std::string const& c, int reversals);

            void add_counter(std::string const& c, int reversals) {
                counters.insert(c);
                set_reversals(c, reversals);
            }

            void add_freevariable(std::string const& fv) {
                freevariables.insert(fv);
            }

            int get_reversals(std::string const& c) const;

            bool has_control(std::string const& control) const {
                return (controls.find(control) != controls.end());
            }

            static head make_head(std::string const& control, std::string const& stack) {
                return std::make_pair(control, stack);
            }

       
            friend std::ostream& operator<<(std::ostream& output, Pds const& pds);

            void remove_unreachable();


        private:
            void update_info(rule_const_ptr r);
            void add_rule_to_map(std::map<head, std::set<rule_const_ptr>>& m,
                                 head h,
                                 rule_const_ptr r);
            void add_rule_to_maps(rule_const_ptr r);
            void del_rule_from_map(std::map<head, std::set<rule_const_ptr>>& m,
                                   head h,
                                   rule_const_ptr r);
            void del_rule_from_maps(rule_const_ptr r);
            bool has_rule_match(rule_const_ptr r);
            bool rule_match(rule_const_ptr r1, rule_const_ptr r2);
            void get_reachable_heads(std::string const& init_p, 
                                     std::string const& init_a, 
                                     std::set<head>& reach_heads);
            void remove_unreachable(std::string const& init_p, std::string const& init_a);
            void rebuild_info();
    };

}

#endif

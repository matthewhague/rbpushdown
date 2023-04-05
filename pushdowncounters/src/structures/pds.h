
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

using namespace std;

string const E_ACT("z");

namespace pds {

    class RuleGuard;
    class RuleGuardVisitor;
    class RGBool;
    class RGNonZero;
    class RGZero;
    class RGNeg;
    class RGImplies;
    class RGDisj;
    class RGConj;

    typedef boost::shared_ptr<RuleGuard> rule_guard_ptr;

    class RuleGuard {
        public:
            virtual void to_stream(ostream& output) const = 0;
            friend ostream& operator<<(ostream& output,  RuleGuard const& f) {
                f.to_stream(output);
                return output;
            }
            friend ostream& operator<<(ostream& output,  rule_guard_ptr const& f) {
                f->to_stream(output);
                return output;
            }

            virtual void accept(RuleGuardVisitor& v) = 0;
    };


    class RuleGuardVisitor {
        public:
            virtual void visit(RGBool& f) = 0;
            virtual void visit(RGNonZero& f) = 0;
            virtual void visit(RGZero& f) = 0;
            virtual void visit(RGNeg& f) = 0;
            virtual void visit(RGImplies& f) = 0;
            virtual void visit(RGDisj& f) = 0;
            virtual void visit(RGConj& f) = 0;
    };




    class RGBool : public RuleGuard {
        bool value;

        public:
            RGBool(bool new_value) { value = new_value; }
            ~RGBool() {}

            virtual void to_stream(ostream& output) const { output << value; }
            bool get_value() const { return value; }

            virtual void accept(RuleGuardVisitor& v) { v.visit(*this); }
    };


    class RGAssocBin : public RuleGuard {
        vector<rule_guard_ptr> clauses;
    
        public:
            RGAssocBin(vector<rule_guard_ptr> const& new_clauses) : clauses(new_clauses) {}
            ~RGAssocBin() {}

            vector<rule_guard_ptr> const& get_clauses() const { return clauses; }
            virtual void to_stream(ostream& output) const;

        private:
            virtual string get_connective() const = 0;
    };


    class RGConj : public RGAssocBin {
        public:
            RGConj(vector<rule_guard_ptr> const& conjuncts) : RGAssocBin(conjuncts) {}
            virtual void accept(RuleGuardVisitor& v) { v.visit(*this); }
        private:
            virtual string get_connective() const { return "&&"; }
    };

    class RGDisj : public RGAssocBin {
        public:
            RGDisj(vector<rule_guard_ptr> const& disjuncts) : RGAssocBin(disjuncts) {}
            virtual void accept(RuleGuardVisitor& v) { v.visit(*this); }
        private:
            virtual string get_connective() const { return "||"; }
    };


    class RGImplies : public RuleGuard {
        rule_guard_ptr lhs;
        rule_guard_ptr rhs;

        public:
            RGImplies(rule_guard_ptr new_lhs, rule_guard_ptr new_rhs) 
                : lhs(new_lhs), rhs(new_rhs) {};

            virtual void to_stream(ostream& output) const { 
                output << "(" << lhs << ") => (" << rhs << ")";
            }
            virtual void accept(RuleGuardVisitor& v) { v.visit(*this); }

            rule_guard_ptr get_lhs() const { return lhs; }
            rule_guard_ptr get_rhs() const { return rhs; }
    };

    class RGNeg : public RuleGuard {
        rule_guard_ptr fmla;

        public:
            RGNeg(rule_guard_ptr new_fmla) : fmla(new_fmla) {};

            virtual void to_stream(ostream& output) const { 
                output << "!(" << fmla << ")";
            }
            virtual void accept(RuleGuardVisitor& v) { v.visit(*this); }

            rule_guard_ptr get_fmla() { return fmla; }
    };

    class RGZero : public RuleGuard {
        string counter_name;

        public:
            RGZero(string const& new_counter_name) : counter_name(new_counter_name) {}
            ~RGZero() {}

            virtual void to_stream(ostream& output) const { 
                output << "(" << counter_name << " == 0)";
            }
            virtual void accept(RuleGuardVisitor& v) { v.visit(*this); }
            string const& get_counter() { return counter_name; }
    };

    class RGNonZero : public RuleGuard {
        string counter_name;

        public:
            RGNonZero(string const& new_counter_name) : counter_name(new_counter_name) {}
            ~RGNonZero() {}

            virtual void to_stream(ostream& output) const { 
                output << "(" << counter_name << " > 0)";
            }
            virtual void accept(RuleGuardVisitor& v) { v.visit(*this); }
            string const& get_counter() { return counter_name; }
    };


    class Rule {
        static int next_id;
        int id; // for
        string p;
        string a;
        string action;
        string q;
        vector<string> w;
        rule_guard_ptr guard;
        set<string> incs;
        set<string> decs;

        public:
            Rule(string const& p, 
                 string const& a,
                 string const& action,
                 string const& q, 
                 vector<string> const& w,
                 rule_guard_ptr const& guard, 
                 set<string> const& incs,
                 set<string> const& decs);
            ~Rule();

            int get_id() const { return id; }
            string const& get_p() const { return p; }
            string const& get_a() const { return a; }
            string const& get_q() const { return q; }
            vector<string> const& get_w() const { return w; }
            string const& get_action() const { return action; }
            set<string> const& get_incs() const { return incs; }
            set<string> const& get_decs() const { return decs; }
            rule_guard_ptr get_guard() const { return guard; }

            bool operator<(Rule const& rhs) const { return id < rhs.get_id(); }
            friend ostream& operator<<(ostream& output,  Rule const& r);
    };


    typedef boost::shared_ptr<Rule const> rule_const_ptr;
    bool operator<(rule_const_ptr lhs, rule_const_ptr rhs);

    class Pds;

    typedef boost::shared_ptr<Pds> pds_ptr;
    typedef boost::shared_ptr<Pds const> pds_const_ptr;


    class Pds {
        set<rule_const_ptr> rules;
        set<string> controls;
        set<string> alphabet;
        set<string> actions;
        set<string> counters;

        map<pair<string, string>, set<rule_const_ptr>> rule_lookup;
        map<pair<string, string>, set<rule_const_ptr>> rule_rev_lookup;
       
        public:
            Pds();
            Pds(set<rule_const_ptr> const& rules);
            ~Pds();

            bool add_rule(rule_const_ptr r);
            void del_rule(rule_const_ptr r);

            set<rule_const_ptr> const& get_rules() const { return rules; }
            set<string> const& get_controls() const { return controls; }
            set<string> const& get_alphabet() const { return alphabet; }
            set<string> const& get_counters() const { return counters; }
            set<string> const& get_actions() const { return actions; }
        
            void minimise(string const& init_p, string const& init_a);

            friend ostream& operator<<(ostream& output, Pds const& pds);


        private:
            void update_info(rule_const_ptr r);
            void update_maps(rule_const_ptr r);
            bool has_rule_match(rule_const_ptr r);
            bool rule_match(rule_const_ptr r1, rule_const_ptr r2);
            void get_reachable_heads(string const& init_p, 
                                     string const& init_a, 
                                     set<pair<string, string>>& reach_heads);
            void remove_unreachable(string const& init_p, string const& init_a);
            void compress_rules();
            void rebuild_info();
            void get_unsilent_heads(set<pair<string, string>>& unsilent_heads);
            void get_counts_in(map<pair<string, string>, int>& counts_in);
    };

}

#endif

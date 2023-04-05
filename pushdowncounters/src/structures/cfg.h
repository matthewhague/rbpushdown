
#pragma once

#include <vector>
#include <set>
#include <string>
#include <map>

#include "boost/shared_ptr.hpp"

#include "pds.h"

using namespace std;

namespace cfg {

    class Rule;

    typedef boost::shared_ptr<Rule const> rule_const_ptr;
    typedef boost::shared_ptr<Rule> rule_ptr;

    struct RuleInfo {
        pds::rule_const_ptr assoc_rule;
        bool change;
    };

    typedef boost::shared_ptr<RuleInfo const> rule_info_const_ptr;
    typedef boost::shared_ptr<RuleInfo> rule_info_ptr;

    class Rule {
        static int next_id; 

        int id;
        string head;
        vector<string> body;
        rule_info_const_ptr info;

        public:
            Rule(string const& new_head, vector<string> const& new_body);
            ~Rule() {};

            string const get_head() const { return head; }
            vector<string> const get_body() const { return body; }
            rule_info_const_ptr get_info() const { return info; }
            int get_id() const { return id; }

            void add_info(rule_info_const_ptr& new_info) { info = new_info; }

            bool operator<(Rule const& rhs) const;
            friend ostream& operator<<(ostream& output,  Rule const& r);

            int count_in_body(string const& nt) const;
    };

    bool operator<(rule_const_ptr lhs, rule_const_ptr rhs);

    class CFG;

    typedef boost::shared_ptr<CFG> cfg_ptr;
    typedef boost::shared_ptr<CFG const> cfg_const_ptr;

    class CFG {
        vector<rule_const_ptr> rules;
        set<string> nonterminals;
        set<string> terminals;
        map<string, set<rule_const_ptr>> head_map;
        map<string, set<rule_const_ptr>> body_map;

        public:
            CFG(vector<rule_const_ptr> const& rules,
                set<string> const& nonterminals,
                set<string> const& terminals);
            CFG(vector<rule_const_ptr> const& rules);
            CFG() {};
            ~CFG() {};

            vector<rule_const_ptr> const& get_rules() const { return rules; }
            set<string> const& get_nonterminals() const { return nonterminals; }
            set<string> const& get_terminals() const { return terminals; }

            bool has_nonterminal(string const& n) const { 
                return nonterminals.find(n) != nonterminals.end();
            }

            void add_rule(rule_const_ptr rule);
            void add_nonterminal(string const& nonterm);
            void add_terminal(string const& term);
            void minimise_cfg(set<string> const& initials);

            set<rule_const_ptr> const& rules_by_head(string const& head) const { 
                return safe_lookup(head, head_map); 
            }
            set<rule_const_ptr> const& rules_by_body(string const& body_elem) const { 
                return safe_lookup(body_elem, body_map); 
            }

            friend ostream& operator<<(ostream& output,  CFG const& g);

        private:
           
            set<rule_const_ptr> empty_rule_vec;
            set<rule_const_ptr> const& 
                safe_lookup(string const& elem, 
                            map<string, set<rule_const_ptr>> const& map) const;
            void add_rule_to_maps(rule_const_ptr rule);
            void add_string_rule_map(string const& s, 
                                     rule_const_ptr& rule, 
                                     map<string, set<rule_const_ptr>>& map);

            void rebuild_rules_maps();
            void remake_with_rules(vector<rule_const_ptr> const& new_rules,
                                   set<string> const& old_nonterms);
    };

}


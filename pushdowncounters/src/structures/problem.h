
#pragma once

#include <fstream>

#include "pds.h"
#include "epresburger.h"

class Problem {
    pds::Pds pds;
    string init_control;
    string init_char;
    string final_control;
    pres::pres_ptr constraint;
    int nreversals;

    public:
        Problem() {};
        ~Problem(); 

        pds::Pds const& get_pds() const { return pds; }
        string const& get_init_control() const { return init_control; }
        string const& get_init_char() const { return init_char; }
        string const& get_final_control() const { return final_control; }
        pres::pres_ptr get_constraint() const { return constraint; }
        int get_nreversals() const { return nreversals; }

        void set_init_control(string const& c) { init_control = c; }
        void set_init_char(string const& c) { init_char = c; } 
        void set_final_control(string const& c) { final_control = c; }
        void set_constraint(pres::pres_ptr& new_c) { constraint = new_c; }
        void set_reversals(int n) { nreversals = n; }

        void add_pds_rule(pds::rule_const_ptr rule) { pds.add_rule(rule); }

        void minimise_pds() { pds.minimise(init_control, init_char); }


        bool parse_problem(string const& description);
        bool parse_problem_file(string const& file_name);

        friend ostream& operator<<(ostream& output, Problem const& prob);
};


#pragma once

#include <iostream>
#include <vector>
#include <set>

#include "boost/shared_ptr.hpp"

using namespace std;

namespace pres {

    class EPresburger;
    class EPAssocBin;
    class EPExists;
    class EPLessThan;
    class EPLessThanEq;
    class EPEqual;
    class EPConj;
    class EPDisj;
    class EPNeg;
    class EPBool;
    class EPImplies;

    class EPVal;
    class EPVar;
    class EPPlus;
    class EPConstMult;
    class EPInteger;
    class EPMinus;

    typedef boost::shared_ptr<EPresburger> pres_ptr;
    typedef boost::shared_ptr<EPVar> pres_var_ptr;
    typedef boost::shared_ptr<EPAssocBin> pres_assoc_bin_ptr;
    typedef boost::shared_ptr<EPVal> pres_val_ptr;
    typedef boost::shared_ptr<EPPlus> pres_plus_ptr;
    typedef boost::shared_ptr<EPExists> pres_exists_ptr;

    class EPresburgerVisitor {
        public:
            virtual void visit(EPLessThan& obj) = 0;
            virtual void visit(EPLessThanEq& obj) = 0;
            virtual void visit(EPEqual& obj) = 0;
            virtual void visit(EPImplies& obj) = 0;
            virtual void visit(EPConj& obj) = 0;
            virtual void visit(EPDisj& obj) = 0;
            virtual void visit(EPNeg& obj) = 0;
            virtual void visit(EPExists& obj) = 0;
            virtual void visit(EPBool& obj) = 0;
    };


    class EPValVisitor {
        public:
            virtual void visit(EPVar& obj) = 0;
            virtual void visit(EPPlus& obj) = 0;
            virtual void visit(EPConstMult& obj) = 0;
            virtual void visit(EPInteger& obj) = 0;
            virtual void visit(EPMinus& obj) = 0;
    };

    class EPresburger {
        public:
            virtual void to_stream(ostream& output) const = 0;
            friend ostream& operator<<(ostream& output,  EPresburger const& f) {
                f.to_stream(output);
                return output;
            }
            friend ostream& operator<<(ostream& output,  pres_ptr const& f) {
                f->to_stream(output);
                return output;
            }
            virtual int num_vars() const = 0;
            virtual void accept(EPresburgerVisitor& v) = 0;
    };





    class EPBool : public EPresburger {
        bool value;

        public:
            EPBool(bool new_value) { value = new_value; }
            ~EPBool() {}

            virtual void to_stream(ostream& output) const { output << value; }

            bool get_value() const { return value; }
            virtual int num_vars() const { return 0; }

            virtual void accept(EPresburgerVisitor& v) { v.visit(*this); }
    };


    class EPAssocBin : public EPresburger {
        vector<pres_ptr> clauses;
    
        public:
            EPAssocBin() {}
            EPAssocBin(vector<pres_ptr> const& new_clauses) : clauses(new_clauses) {}
            EPAssocBin(pres_ptr& lhs, pres_ptr& rhs);
            ~EPAssocBin() {}

            vector<pres_ptr> const& get_clauses() const { return clauses; }
            virtual void to_stream(ostream& output) const;
            void add_clause(pres_ptr new_clause) { clauses.push_back(new_clause); }
            virtual int num_vars() const;

        private:
            virtual string get_connective() const = 0;
    };

    class EPNeg : public EPresburger {
        pres_ptr fmla;

        public:
            EPNeg(pres_ptr& new_fmla) : fmla(new_fmla) {}
            ~EPNeg() {}

            virtual void to_stream(ostream& output) const {
                output << "!(" << fmla << ")";
            }

            virtual int num_vars() const { return fmla->num_vars(); }
            pres_ptr& get_fmla() { return fmla; }


            virtual void accept(EPresburgerVisitor& v) { v.visit(*this); }
    };


    class EPConj : public EPAssocBin {
        public:
            EPConj() {}
            EPConj(vector<pres_ptr> const& conjuncts) : EPAssocBin(conjuncts) {}
            EPConj(pres_ptr& lhs, pres_ptr& rhs) : EPAssocBin(lhs, rhs) {}
        private:
            virtual string get_connective() const { return "&&"; }
            virtual void accept(EPresburgerVisitor& v) { v.visit(*this); }
    };

    class EPDisj : public EPAssocBin {
        public:
            EPDisj() {}
            EPDisj(vector<pres_ptr> const& disjuncts) : EPAssocBin(disjuncts) {}
            EPDisj(pres_ptr& lhs, pres_ptr& rhs) : EPAssocBin(lhs, rhs) {}
        private:
            virtual string get_connective() const { return "||"; }
            virtual void accept(EPresburgerVisitor& v) { v.visit(*this); }
    };

    class EPVal {
        public:
            virtual void to_stream(ostream& output) const = 0;
            friend ostream& operator<<(ostream& output,  EPVal const& f) {
                f.to_stream(output);
                return output;
            }
            friend ostream& operator<<(ostream& output,  pres_val_ptr const& f) {
                f->to_stream(output);
                return output;
            }

            virtual void accept(EPValVisitor& v) = 0;
    };



    class EPVar : public EPVal {
        string name;

        public:
            EPVar(string const& new_name) : name(new_name) {}
            ~EPVar() {}

            string const& get_name() const { return name; }
            
            virtual void accept(EPValVisitor& v) { v.visit(*this); }
            virtual void to_stream(ostream& output) const { output << name; }
    };

    class EPExists : public EPresburger {
        set<string> vars;
        pres_ptr fmla;

        public:
            EPExists(set<string> const& new_vars, pres_ptr& new_fmla) 
                : vars(new_vars), fmla(new_fmla) { }
            EPExists(pres_ptr& new_fmla) 
                : fmla(new_fmla) { }
            ~EPExists() {}

            void add_var(pres_var_ptr& new_var) { vars.insert(new_var->get_name()); }
            set<string> const& get_vars() const { return vars; } 
            pres_ptr& get_fmla() { return fmla; }
            virtual int num_vars() const { return vars.size() + fmla->num_vars(); }
            virtual void accept(EPresburgerVisitor& v) { v.visit(*this); }

            void to_stream(ostream& output) const;
    };


    class EPImplies : public EPresburger {
        pres_ptr lhs;
        pres_ptr rhs;

        public:
            EPImplies(pres_ptr new_lhs, pres_ptr new_rhs) 
                : lhs(new_lhs), rhs(new_rhs) {};

            virtual void to_stream(ostream& output) const { 
                output << "(" << lhs << ") => (" << rhs << ")";
            }

            pres_ptr& get_lhs() { return lhs; }
            pres_ptr& get_rhs() { return rhs; }

            virtual int num_vars() const { return lhs->num_vars() + rhs->num_vars(); }
            virtual void accept(EPresburgerVisitor& v) { v.visit(*this); }
    };



    class EPCompare : public EPresburger {
        pres_val_ptr lhs;
        pres_val_ptr rhs;

        public:
            EPCompare(pres_val_ptr& new_lhs, pres_val_ptr& new_rhs) : lhs(new_lhs), rhs(new_rhs) {}
            ~EPCompare() {}

            virtual void to_stream(ostream& output) const {
                output << "(" << lhs << ") " << get_connective() << " (" << rhs << ")";
            }

            pres_val_ptr get_lhs() const { return lhs; }
            pres_val_ptr get_rhs() const { return rhs; }
        
        private:
            virtual string get_connective() const = 0;
            virtual int num_vars() const { return 0; }
    };

    class EPEqual : public EPCompare {
        public:
            EPEqual(pres_val_ptr& lhs, pres_val_ptr& rhs) : EPCompare(lhs, rhs) {}
            virtual void accept(EPresburgerVisitor& v) { v.visit(*this); }
        private:
            virtual string get_connective() const { return "="; }
    };

    class EPLessThanEq : public EPCompare {
        public:
            EPLessThanEq(pres_val_ptr& lhs, pres_val_ptr& rhs) : EPCompare(lhs, rhs) {}
            virtual void accept(EPresburgerVisitor& v) { v.visit(*this); }
        private:
            virtual string get_connective() const { return "<="; }
    };

    class EPLessThan : public EPCompare {
        public:
            EPLessThan(pres_val_ptr& lhs, pres_val_ptr& rhs) : EPCompare(lhs, rhs) {}
            virtual void accept(EPresburgerVisitor& v) { v.visit(*this); }
        private:
            virtual string get_connective() const { return "<"; }
    };


    class EPInteger : public EPVal {
        int value;

        public:
            EPInteger(int new_value) { value = new_value; }
            ~EPInteger() {}

            int get_value() const { return value; }

            virtual void accept(EPValVisitor& v) { v.visit(*this); }
            virtual void to_stream(ostream& output) const { output << value; }
    };


    class EPPlus : public EPVal {
        vector<pres_val_ptr> operands;

        public:
            EPPlus() {}
            EPPlus(vector<pres_val_ptr> const& new_ops) : operands(new_ops) {}
            ~EPPlus() {}

            virtual void to_stream(ostream& output) const;

            vector<pres_val_ptr> const& get_operands() const { return operands; }
            void add_operand(pres_val_ptr new_operand) {
                operands.push_back(new_operand);
            }
            virtual void accept(EPValVisitor& v) { v.visit(*this); }
    };

    class EPMinus : public EPVal {
        pres_val_ptr lhs;
        pres_val_ptr rhs;

        public:
            EPMinus(pres_val_ptr new_lhs, pres_val_ptr new_rhs) 
                : lhs(new_lhs), rhs(new_rhs) {};

            virtual void to_stream(ostream& output) const { 
                output << "(" << lhs << ") - (" << rhs << ")";
            }

            pres_val_ptr& get_lhs() { return lhs; }
            pres_val_ptr& get_rhs() { return rhs; }

            virtual void accept(EPValVisitor& v) { v.visit(*this); }
    };


    class EPConstMult : public EPVal {
        int scalar;
        pres_val_ptr scalee;

        public:
            EPConstMult(int new_scalar, pres_val_ptr new_scalee)
                : scalar(new_scalar), scalee(new_scalee) {}
            ~EPConstMult() {}

            int get_scalar() const { return scalar; }
            pres_val_ptr& get_scalee() { return scalee; }

            virtual void accept(EPValVisitor& v) { v.visit(*this); }
            virtual void to_stream(ostream& output) const {
                output << scalar << "*(" << scalee << ")";
            }
                  
    };



}


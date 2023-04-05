
#pragma once
#ifndef __EPRESBURGER_H__
#define __EPRESBURGER_H__

#include <iostream>
#include <vector>
#include <set>

#include "boost/shared_ptr.hpp"

#include "boolexpression.h"

namespace pres {

    class Environment;

    class EPresVisitor;
    class EPresValVisitor;
    class EPresExists;

    typedef expr::BoolExpression<Environment, EPresVisitor>        EPresburger;
    typedef expr::BoolExpressionConst<Environment, EPresVisitor>   EPresConst;
    typedef expr::BoolExpressionAtom<Environment, EPresVisitor>    EPresAtom;
    typedef expr::BoolExpressionBinOp<Environment, EPresVisitor>   EPresBinOp;
    typedef expr::BoolExpressionAnd<Environment, EPresVisitor>     EPresAnd;
    typedef expr::BoolExpressionOr<Environment, EPresVisitor>      EPresOr;
    typedef expr::BoolExpressionImplies<Environment, EPresVisitor> EPresImplies;
    typedef expr::BoolExpressionNot<Environment, EPresVisitor>     EPresNot;
    class EPresCompare;
    class EPresLessThan;
    class EPresLessThanEq;
    class EPresEqual;

    class EPresVal;
    class EPresVar;
    class EPresPlus;
    class EPresConstMult;
    class EPresInteger;
    class EPresMinus;

    typedef boost::shared_ptr<Environment> environment_ptr;

    typedef expr::Ptrs<Environment, EPresVisitor>::boolexpression_ptr  epresburger_ptr;
    typedef expr::Ptrs<Environment, EPresVisitor>::boolexp_const_ptr   epres_const_ptr;
    typedef expr::Ptrs<Environment, EPresVisitor>::boolexp_atom_ptr    epres_atom_ptr;
    typedef expr::Ptrs<Environment, EPresVisitor>::boolexp_and_ptr     epres_and_ptr;
    typedef expr::Ptrs<Environment, EPresVisitor>::boolexp_or_ptr      epres_or_ptr;
    typedef expr::Ptrs<Environment, EPresVisitor>::boolexp_implies_ptr epres_implies_ptr;
    typedef expr::Ptrs<Environment, EPresVisitor>::boolexp_not_ptr     epres_not_ptr;
    typedef boost::shared_ptr<EPresCompare>    epres_compare_ptr;
    typedef boost::shared_ptr<EPresVar>        epres_var_ptr;
    typedef boost::shared_ptr<EPresVal>        epres_val_ptr;
    typedef boost::shared_ptr<EPresPlus>       epres_plus_ptr;
    typedef boost::shared_ptr<EPresExists>     epres_exists_ptr;

    typedef boost::shared_ptr<EPresVal>       epres_val_ptr;
    typedef boost::shared_ptr<EPresVar>       epres_var_ptr;
    typedef boost::shared_ptr<EPresPlus>      epres_plus_ptr;
    typedef boost::shared_ptr<EPresConstMult> epres_constmult_ptr;
    typedef boost::shared_ptr<EPresInteger>   epres_integer_ptr;
    typedef boost::shared_ptr<EPresMinus>     epres_minus_ptr;


    class EPresVisitor {
        public:
            virtual void visit(EPresCompare& obj) = 0;
            virtual void visit(EPresImplies& obj) = 0;
            virtual void visit(EPresAnd& obj) = 0;
            virtual void visit(EPresOr& obj) = 0;
            virtual void visit(EPresNot& obj) = 0;
            virtual void visit(EPresExists& obj) = 0;
            virtual void visit(EPresConst& obj) = 0;
    };


    class EPresValVisitor {
        public:
            virtual void visit(EPresVar& obj) = 0;
            virtual void visit(EPresPlus& obj) = 0;
            virtual void visit(EPresConstMult& obj) = 0;
            virtual void visit(EPresInteger& obj) = 0;
            virtual void visit(EPresMinus& obj) = 0;
    };

    class Environment {
        // dummy environment since we'll never evaluate
    };



    class EPresVal {
        public:
            virtual void to_stream(std::ostream& output) const = 0;
            friend std::ostream& operator<<(std::ostream& output,  EPresVal const& f) {
                f.to_stream(output);
                return output;
            }
            friend std::ostream& operator<<(std::ostream& output,  epres_val_ptr const& f) {
                f->to_stream(output);
                return output;
            }

            virtual void accept(EPresValVisitor& v) = 0;
            virtual epres_val_ptr make_shared_ptr() = 0;
    };



    class EPresVar : public EPresVal {
        std::string name;

        public:
            EPresVar(std::string const& new_name) : name(new_name) {}
            ~EPresVar() {}

            std::string const& get_name() const { return name; }
            
            virtual void accept(EPresValVisitor& v) { v.visit(*this); }
            virtual void to_stream(std::ostream& output) const { output << name; }

            virtual epres_val_ptr make_shared_ptr() {
                epres_var_ptr p(this);
                return p;
            }
    };


    enum CompOp { EQ, NEQ, GT, LT, GTE, LTE };

    class EPresCompare : public EPresAtom {
        epres_val_ptr lhs;
        CompOp op;
        epres_val_ptr rhs;

        public:
            EPresCompare(epres_val_ptr new_lhs, 
                         CompOp new_op,
                         epres_val_ptr new_rhs) 
                : lhs(new_lhs), 
                  op(new_op),  
                  rhs(new_rhs) {}
            ~EPresCompare() {}


            epres_val_ptr get_lhs() const { return lhs; }
            epres_val_ptr get_rhs() const { return rhs; }
            CompOp get_op() const { return op; }

            virtual void to_stream(std::ostream& output) const; 
            virtual void accept(EPresVisitor& v) { v.visit(*this); }
        
            virtual bool evaluate(Environment const& env) const {
                return true;
            }

            virtual epresburger_ptr make_shared_ptr() {
                epres_compare_ptr p(this);
                return p;
            }

            virtual epresburger_ptr normal_form(bool negative);
    };


    class EPresInteger : public EPresVal {
        int value;

        public:
            EPresInteger(int new_value) { value = new_value; }
            ~EPresInteger() {}

            int get_value() const { return value; }

            virtual void accept(EPresValVisitor& v) { v.visit(*this); }
            virtual void to_stream(std::ostream& output) const { output << value; }

            virtual epres_val_ptr make_shared_ptr() {
                epres_integer_ptr p(this);
                return p;
            }
    };


    class EPresPlus : public EPresVal {
        std::vector<epres_val_ptr> operands;

        public:
            EPresPlus() {}
            EPresPlus(epres_val_ptr op1, epres_val_ptr op2) {
                operands.push_back(op1);
                operands.push_back(op2);
            }
            EPresPlus(std::vector<epres_val_ptr> const& new_ops) : operands(new_ops) {}
            ~EPresPlus() {}

            virtual void to_stream(std::ostream& output) const;

            std::vector<epres_val_ptr> const& get_operands() const { return operands; }
            void add_operand(epres_val_ptr new_operand) {
                operands.push_back(new_operand);
            }
            virtual void accept(EPresValVisitor& v) { v.visit(*this); }

            virtual epres_val_ptr make_shared_ptr() {
                epres_plus_ptr p(this);
                return p;
            }
    };

    class EPresMinus : public EPresVal {
        epres_val_ptr lhs;
        epres_val_ptr rhs;

        public:
            EPresMinus(epres_val_ptr new_lhs, epres_val_ptr new_rhs) 
                : lhs(new_lhs), rhs(new_rhs) {};

            virtual void to_stream(std::ostream& output) const { 
                output << "(" << lhs << ") - (" << rhs << ")";
            }

            epres_val_ptr get_lhs() const { return lhs; }
            epres_val_ptr get_rhs() const { return rhs; }

            virtual void accept(EPresValVisitor& v) { v.visit(*this); }

            virtual epres_val_ptr make_shared_ptr() {
                epres_minus_ptr p(this);
                return p;
            }
    };


    class EPresConstMult : public EPresVal {
        int scalar;
        epres_val_ptr scalee;

        public:
            EPresConstMult(int new_scalar, epres_val_ptr new_scalee)
                : scalar(new_scalar), scalee(new_scalee) {}
            ~EPresConstMult() {}

            int get_scalar() const { return scalar; }
            epres_val_ptr get_scalee() { return scalee; }

            virtual void accept(EPresValVisitor& v) { v.visit(*this); }
            virtual void to_stream(std::ostream& output) const {
                output << scalar << "*(" << scalee << ")";
            }
                  
            virtual epres_val_ptr make_shared_ptr() {
                epres_constmult_ptr p(this);
                return p;
            }
    };


    class EPresExists : public EPresburger {
        std::set<std::string> vars;
        epresburger_ptr fmla;

        public:
            EPresExists(std::set<std::string> const& new_vars, epresburger_ptr new_fmla) 
                : vars(new_vars), fmla(new_fmla) { }
            EPresExists(epresburger_ptr new_fmla) 
                : fmla(new_fmla) { }
            ~EPresExists() {}

            void add_var(epres_var_ptr new_var) { vars.insert(new_var->get_name()); }
            std::set<std::string> const& get_vars() const { return vars; } 
            epresburger_ptr get_fmla() { return fmla; }
            virtual void accept(EPresVisitor& v) { v.visit(*this); }

            void to_stream(std::ostream& output) const;

            virtual epresburger_ptr make_shared_ptr() {
                epres_exists_ptr p(this);
                return p;
            }

            virtual bool evaluate(Environment const& env) const {
                return true;
            }

            virtual epresburger_ptr normal_form(bool negative);
    };




}

#endif

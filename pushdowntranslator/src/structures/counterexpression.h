
#pragma once
#ifndef __COUNTEREXPRESSION_H__
#define __COUNTEREXPRESSION_H__

#include "boolexpression.h"

namespace ctrexp {

    class Environment;
    class CExpVisitor;

    typedef expr::BoolExpression<Environment, CExpVisitor>        CounterExpression;
    typedef expr::BoolExpressionConst<Environment, CExpVisitor>   CExpConst;
    typedef expr::BoolExpressionAtom<Environment, CExpVisitor>    CExpAtom;
    typedef expr::BoolExpressionBinOp<Environment, CExpVisitor>   CExpBinOp;
    typedef expr::BoolExpressionAnd<Environment, CExpVisitor>     CExpAnd;
    typedef expr::BoolExpressionOr<Environment, CExpVisitor>      CExpOr;
    typedef expr::BoolExpressionImplies<Environment, CExpVisitor> CExpImplies;
    typedef expr::BoolExpressionNot<Environment, CExpVisitor>     CExpNot;
    class CExpConstCompare;
    class CExpFVCompare;

    typedef boost::shared_ptr<Environment> environment_ptr;

    typedef expr::Ptrs<Environment, CExpVisitor>::boolexpression_ptr  counterexp_ptr;
    typedef expr::Ptrs<Environment, CExpVisitor>::boolexp_const_ptr   cexp_const_ptr;
    typedef expr::Ptrs<Environment, CExpVisitor>::boolexp_atom_ptr    cexp_atom_ptr;
    typedef expr::Ptrs<Environment, CExpVisitor>::boolexp_and_ptr     cexp_and_ptr;
    typedef expr::Ptrs<Environment, CExpVisitor>::boolexp_or_ptr      cexp_or_ptr;
    typedef expr::Ptrs<Environment, CExpVisitor>::boolexp_implies_ptr cexp_implies_ptr;
    typedef expr::Ptrs<Environment, CExpVisitor>::boolexp_not_ptr     cexp_not_ptr;

    typedef boost::shared_ptr<CExpConstCompare> cexp_const_compare_ptr;
    typedef boost::shared_ptr<CExpFVCompare> cexp_fv_compare_ptr;

    // Dummy environment since we'll never evaluate
    class Environment { };

    class CExpVisitor {
        public:
            virtual void visit(CExpConst& b) = 0;
            virtual void visit(CExpAnd& b) = 0;
            virtual void visit(CExpOr& b) = 0;
            virtual void visit(CExpImplies& b) = 0;
            virtual void visit(CExpNot& b) = 0;
            virtual void visit(CExpConstCompare& b) = 0;
            virtual void visit(CExpFVCompare& b) = 0;
    };


    enum CompOp { EQ, NEQ, GT, LT, GTE, LTE };

    class CExpCompare : public CExpAtom {
        std::string variable_name;
        CompOp      op;

        public:
            CExpCompare() { }

            CExpCompare(std::string const& new_variable_name,
                        CompOp new_op)
                : variable_name(new_variable_name),
                  op(new_op) {}
            ~CExpCompare() {}

            void set_variable_name(std::string const& new_variable_name) {
                variable_name = new_variable_name;
            }

            void set_op(CompOp new_op) {
                op = new_op;
            }

            std::string const& get_variable_name() const {
                return variable_name;
            }

            CompOp get_op() const {
                return op;
            }

            virtual void to_stream(std::ostream& output) const;

            virtual bool evaluate(Environment const& env) const {
                return true;
            }

            CompOp normal_form_op(bool neg);

    };

    class CExpConstCompare : public CExpCompare {
        int value;

        public:
            CExpConstCompare() { }

            CExpConstCompare(std::string const& new_variable_name,
                             CompOp new_op,
                             int new_value)
                : CExpCompare(new_variable_name, new_op),
                  value(new_value) {}
            ~CExpConstCompare() {}

            void set_value(int new_value) {
                value = new_value;
            }

            int get_value() const {
                return value;
            }

            virtual void to_stream(std::ostream& output) const;

            virtual counterexp_ptr make_shared_ptr() {
                cexp_const_compare_ptr p(this);
                return p;
            }

            virtual void accept(CExpVisitor& v) { v.visit(*this); }

            virtual counterexp_ptr normal_form(bool negative);
    };



    class CExpFVCompare : public CExpCompare {
        std::string freevariable_name;

        public:
            CExpFVCompare() { }

            CExpFVCompare(std::string const& new_variable_name,
                          CompOp new_op,
                          std::string const& new_freevariable_name)
                : CExpCompare(new_variable_name, new_op),
                  freevariable_name(new_freevariable_name) {}
            ~CExpFVCompare() {}

            void set_freevariable_name(int new_freevariable_name) {
                freevariable_name = new_freevariable_name;
            }

            std::string const& get_freevariable_name() const {
                return freevariable_name;
            }

            virtual void to_stream(std::ostream& output) const;

            virtual counterexp_ptr make_shared_ptr() {
                cexp_fv_compare_ptr p(this);
                return p;
            }

            virtual void accept(CExpVisitor& v) { v.visit(*this); }

            virtual counterexp_ptr normal_form(bool negative);
    };



}

#endif

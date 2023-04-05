

#pragma once
#ifndef __BOOLEXPRESSION_H__
#define __BOOLEXPRESSION_H__


#include <iostream>
#include <cstdlib>
#include "boost/shared_ptr.hpp"
#include "boost/make_shared.hpp"

namespace expr {

    // use by declaring a dummy class
    //
    // class MyBoolExpr { };
    // and some concrete atoms : class MyAtom : BoolExpressionAtom<MyBoolExpr>
    // then you have BoolExpressions<MyBoolExpr> all subtypes of MyBoolExpr
    // so no-one else can put in dirty atoms from other types of bool
    // expressions...
    //
    // (see program.h for example)

    template <class Environment, class Visitor> class BoolExpression;
    template <class Environment, class Visitor> class BoolExpressionConst;
    template <class Environment, class Visitor> class BoolExpressionAtom;
    template <class Environment, class Visitor> class BoolExpressionBinOp;
    template <class Environment, class Visitor> class BoolExpressionAnd;
    template <class Environment, class Visitor> class BoolExpressionOr;
    template <class Environment, class Visitor> class BoolExpressionImplies;
    template <class Environment, class Visitor> class BoolExpressionNot;

    template<class Environment, class Visitor>
    struct Ptrs
    {
        typedef boost::shared_ptr<BoolExpression<Environment, Visitor>>        boolexpression_ptr;
        typedef boost::shared_ptr<BoolExpressionConst<Environment, Visitor>>   boolexp_const_ptr;
        typedef boost::shared_ptr<BoolExpressionAtom<Environment, Visitor>>    boolexp_atom_ptr;
        typedef boost::shared_ptr<BoolExpressionAnd<Environment, Visitor>>     boolexp_and_ptr;
        typedef boost::shared_ptr<BoolExpressionOr<Environment, Visitor>>      boolexp_or_ptr;
        typedef boost::shared_ptr<BoolExpressionImplies<Environment, Visitor>> boolexp_implies_ptr;
        typedef boost::shared_ptr<BoolExpressionNot<Environment, Visitor>>     boolexp_not_ptr;
    }; 


    template <class Environment, class Visitor>
    class BoolExpression : public Environment {
        public:
            virtual bool evaluate(Environment const& env) const = 0;

            virtual void to_stream(std::ostream& output) const = 0;
            friend std::ostream& operator<<(std::ostream& output,  BoolExpression const& p) {
                p.to_stream(output);
                return output;
            }

            virtual typename Ptrs<Environment, Visitor>::boolexpression_ptr make_shared_ptr() = 0;
            virtual void accept(Visitor& v) = 0;

            virtual typename Ptrs<Environment, Visitor>::boolexpression_ptr normal_form(bool negative) = 0;
    };

    template <class Environment, class Visitor>
    class BoolExpressionConst : public BoolExpression<Environment, Visitor> {
        int value;

        public:
            BoolExpressionConst() { }

            BoolExpressionConst(bool v) : value(v) { }

            void set_value(bool new_value) {
                value = new_value;
            }

            bool get_value() const {
                return value;
            }

            virtual bool evaluate(Environment const& env) const {
                return get_value();
            }

            virtual void to_stream(std::ostream& output) const{
                output << (value ? "tt" : "ff"); 
            }

            virtual typename Ptrs<Environment, Visitor>::boolexpression_ptr make_shared_ptr() {
                typename Ptrs<Environment, Visitor>::boolexp_const_ptr p(this);
                return p;
            }

            virtual void accept(Visitor& v) {
                v.visit(*this);
            }

            virtual typename Ptrs<Environment, Visitor>::boolexpression_ptr normal_form(bool negative) {
                int new_val = negative ? !get_value() : get_value();
                return boost::make_shared<BoolExpressionConst<Environment, Visitor>>(new_val);
            }
    };

    template <class Environment, class Visitor>
    class BoolExpressionAtom : public BoolExpression<Environment, Visitor> {
        public:
            virtual bool evaluate(Environment const& env) const = 0;
            virtual void to_stream(std::ostream& output) const = 0;
            virtual typename Ptrs<Environment, Visitor>::boolexpression_ptr make_shared_ptr() = 0;
            virtual void accept(Visitor& v) = 0;
            virtual typename Ptrs<Environment, Visitor>::boolexpression_ptr normal_form(bool negative) = 0;
    };

    template <class Environment, class Visitor>
    class BoolExpressionImplies : public BoolExpression<Environment, Visitor> {
        typename Ptrs<Environment, Visitor>::boolexpression_ptr lhs;
        typename Ptrs<Environment, Visitor>::boolexpression_ptr rhs;

        public:
            BoolExpressionImplies<Environment, Visitor>() { }

            BoolExpressionImplies<Environment, Visitor>(
                typename Ptrs<Environment, Visitor>::boolexpression_ptr new_lhs,
                typename Ptrs<Environment, Visitor>::boolexpression_ptr new_rhs) {
                lhs = new_lhs;
                rhs = new_rhs;
            }

            typename Ptrs<Environment, Visitor>::boolexpression_ptr get_lhs() const {
                return lhs;
            }

            typename Ptrs<Environment, Visitor>::boolexpression_ptr get_rhs() const {
                return rhs;
            }

            virtual void to_stream(std::ostream& output) const {
                if (lhs && rhs) {
                    output << "(" << *lhs << " => " << *rhs << ")";
                } else {
                    output << "(implies missing an operand)";
                }
            }

            virtual bool evaluate(Environment const& env) const {
                if (!lhs || !rhs) {
                    std::cerr << "BoolExpressionImplies attempted to evaluate a null expression." << std::endl;
                    exit(-1);
                }
                return !lhs->evaluate(env) || rhs->evaluate(env);
            }

            virtual typename Ptrs<Environment, Visitor>::boolexpression_ptr make_shared_ptr() {
                typename Ptrs<Environment, Visitor>::boolexp_implies_ptr p(this);
                return p;
            }

            virtual void accept(Visitor& v) {
                v.visit(*this);
            }

            virtual typename Ptrs<Environment, Visitor>::boolexpression_ptr normal_form(bool negative) {
                if (!negative) {
                    // !a or b
                    typename Ptrs<Environment, Visitor>::boolexpression_ptr not_a 
                        = get_lhs()->normal_form(true);
                    typename Ptrs<Environment, Visitor>::boolexpression_ptr b 
                        = get_rhs()->normal_form(false);
                    return boost::make_shared<BoolExpressionOr<Environment, Visitor>>(not_a, b);
                } else {
                    // a and !b
                    typename Ptrs<Environment, Visitor>::boolexpression_ptr a 
                        = get_lhs()->normal_form(false);
                    typename Ptrs<Environment, Visitor>::boolexpression_ptr not_b 
                        = get_rhs()->normal_form(true);
                    return boost::make_shared<BoolExpressionAnd<Environment, Visitor>>(a, not_b);
                }
            }
    };


    template <class Environment, class Visitor>
    class BoolExpressionBinOp : public BoolExpression<Environment, Visitor> {
        protected:
            std::vector<typename Ptrs<Environment, Visitor>::boolexpression_ptr> operands;

        public:
            BoolExpressionBinOp<Environment, Visitor>() { }

            BoolExpressionBinOp<Environment, Visitor>(
                typename Ptrs<Environment, Visitor>::boolexpression_ptr lhs,
                typename Ptrs<Environment, Visitor>::boolexpression_ptr rhs) {
                if (lhs && rhs) {
                    operands.push_back(lhs);
                    operands.push_back(rhs);
                }
            }

            void add_operand(typename Ptrs<Environment, Visitor>::boolexpression_ptr new_operand) {
                if (new_operand) 
                    operands.push_back(new_operand);
            }

            void set_operands(std::vector<typename Ptrs<Environment, Visitor>::boolexpression_ptr> const& new_operands) {
                operands = new_operands;
            }

            std::vector<typename Ptrs<Environment, Visitor>::boolexpression_ptr> const& get_operands() const {
                return operands;
            }

            virtual bool evaluate(Environment const& env) const = 0;
            
            virtual void to_stream(std::ostream& output) const{
                output << "(";
                auto it = operands.begin();
                while (it != operands.end()) {
                    output << *(*it);
                    if (++it != operands.end()) {
                        output << " ";
                        op_to_stream(output);
                        output << " ";
                    }
                }
                output << ")";
            }

            virtual typename Ptrs<Environment, Visitor>::boolexpression_ptr make_shared_ptr() = 0;
            virtual void accept(Visitor& v) = 0;

            virtual typename Ptrs<Environment, Visitor>::boolexpression_ptr normal_form(bool negative) = 0;
        protected:
            virtual void op_to_stream(std::ostream& output) const = 0;
    };

    template <class Environment, class Visitor>
    class BoolExpressionAnd : public BoolExpressionBinOp<Environment, Visitor> { 
        public:
            BoolExpressionAnd<Environment, Visitor>() { }

            BoolExpressionAnd<Environment, Visitor>(
                typename Ptrs<Environment, Visitor>::boolexpression_ptr lhs,
                typename Ptrs<Environment, Visitor>::boolexpression_ptr rhs) 
                : BoolExpressionBinOp<Environment, Visitor>(lhs, rhs) { }


            virtual bool evaluate(Environment const& env) const {
                bool result = true;
                auto it = this->operands.begin();
                while (result && it != this->operands.end()) {
                    result = (*it)->evaluate(env);
                    ++it;
                }
                return result;
            }

            virtual typename Ptrs<Environment, Visitor>::boolexpression_ptr make_shared_ptr() {
                typename Ptrs<Environment, Visitor>::boolexp_and_ptr p(this);
                return p;
            }

            virtual void accept(Visitor& v) {
                v.visit(*this);
            }

            virtual typename Ptrs<Environment, Visitor>::boolexpression_ptr normal_form(bool negative) {
                if (!negative) {
                    typename Ptrs<Environment, Visitor>::boolexp_and_ptr f =
                        boost::make_shared<BoolExpressionAnd<Environment, Visitor>>();
                    for (typename Ptrs<Environment, Visitor>::boolexpression_ptr sub_f :
                             this->get_operands()) {
                        typename Ptrs<Environment, Visitor>::boolexpression_ptr op 
                            = sub_f->normal_form(false);
                        f->add_operand(op);
                    }
                    return f;
                } else {
                    typename Ptrs<Environment, Visitor>::boolexp_or_ptr f =
                        boost::make_shared<BoolExpressionOr<Environment, Visitor>>();
                    for (typename Ptrs<Environment, Visitor>::boolexpression_ptr sub_f :
                             this->get_operands()) {
                        typename Ptrs<Environment, Visitor>::boolexpression_ptr op 
                            = sub_f->normal_form(true);
                        f->add_operand(op);
                    }
                    return f;
                }
            }

        protected:
            virtual void op_to_stream(std::ostream& output) const {
                output << "&&";
            }
    };

    template <class Environment, class Visitor>
    class BoolExpressionOr : public BoolExpressionBinOp<Environment, Visitor> {
        public:
            BoolExpressionOr<Environment, Visitor>() { }

            BoolExpressionOr<Environment, Visitor>(
                typename Ptrs<Environment, Visitor>::boolexpression_ptr lhs,
                typename Ptrs<Environment, Visitor>::boolexpression_ptr rhs) 
                : BoolExpressionBinOp<Environment, Visitor>(lhs, rhs) { }

            virtual bool evaluate(Environment const& env) const {
                bool result = false;
                auto it = this->operands.begin();
                while (!result && it != this->operands.end()) {
                    result = (*it)->evaluate(env);
                    ++it;
                }
                return result;
            }

            virtual typename Ptrs<Environment, Visitor>::boolexpression_ptr make_shared_ptr() {
                typename Ptrs<Environment, Visitor>::boolexp_or_ptr p(this);
                return p;
            }

            virtual void accept(Visitor& v) {
                v.visit(*this);
            }

            virtual typename Ptrs<Environment, Visitor>::boolexpression_ptr normal_form(bool negative) {
                if (!negative) {
                    typename Ptrs<Environment, Visitor>::boolexp_or_ptr f =
                        boost::make_shared<BoolExpressionOr<Environment, Visitor>>();
                    for (typename Ptrs<Environment, Visitor>::boolexpression_ptr sub_f :
                             this->get_operands()) {
                        typename Ptrs<Environment, Visitor>::boolexpression_ptr op 
                            = sub_f->normal_form(false);
                        f->add_operand(op);
                    }
                    return f;
                } else {
                    typename Ptrs<Environment, Visitor>::boolexp_and_ptr f =
                        boost::make_shared<BoolExpressionAnd<Environment, Visitor>>();
                    for (typename Ptrs<Environment, Visitor>::boolexpression_ptr sub_f :
                             this->get_operands()) {
                        typename Ptrs<Environment, Visitor>::boolexpression_ptr op 
                            = sub_f->normal_form(true);
                        f->add_operand(op);
                    }
                    return f;
                }
            }
        protected:
            virtual void op_to_stream(std::ostream& output) const {
                output << "||";
            }
    };

    template <class Environment, class Visitor>
    class BoolExpressionNot : public BoolExpression<Environment, Visitor> {
        typename Ptrs<Environment, Visitor>::boolexpression_ptr expr;

        public:
            BoolExpressionNot<Environment, Visitor>(
                typename Ptrs<Environment, Visitor>::boolexpression_ptr new_expr) 
                : expr(new_expr) { }


            void set_expression(typename Ptrs<Environment, Visitor>::boolexpression_ptr new_expr) {
                expr = new_expr;
            }

            typename Ptrs<Environment, Visitor>::boolexpression_ptr get_expression() {
                return expr;
            }

            virtual bool evaluate(Environment const& env) const {
                if (!expr) {
                    std::cerr << "BoolExpressionNot attempted to evaluate a null expression." << std::endl;
                    exit(-1);
                }
                return !expr->evaluate(env);
            }

            virtual void to_stream(std::ostream& output) const {
                if (expr)
                    output << "!(" << (*expr) << ")";
                else
                    output << "!(no expression)";
            }

            virtual typename Ptrs<Environment, Visitor>::boolexpression_ptr make_shared_ptr() {
                typename Ptrs<Environment, Visitor>::boolexp_not_ptr p(this);
                return p;
            }

            virtual void accept(Visitor& v) {
                v.visit(*this);
            }

            virtual typename Ptrs<Environment, Visitor>::boolexpression_ptr normal_form(bool negative) {
                return get_expression()->normal_form(!negative);
            }
    };


}

#endif

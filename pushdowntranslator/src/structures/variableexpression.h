
#pragma once
#ifndef __VARIABLEEXPRESSION_H__
#define __VARIABLEEXPRESSION_H__

#include <map>
#include <vector>
#include <cmath>

#include "boolexpression.h"

namespace vexp {

    int const NULL_HASH = 0;

    class Environment;
    class VExpVisitor;

    typedef expr::BoolExpression<Environment, VExpVisitor>        VarExpression;
    typedef expr::BoolExpressionConst<Environment, VExpVisitor>   VExpConst;
    typedef expr::BoolExpressionAtom<Environment, VExpVisitor>    VExpAtom;
    typedef expr::BoolExpressionBinOp<Environment, VExpVisitor>   VExpBinOp;
    typedef expr::BoolExpressionAnd<Environment, VExpVisitor>     VExpAnd;
    typedef expr::BoolExpressionOr<Environment, VExpVisitor>      VExpOr;
    typedef expr::BoolExpressionImplies<Environment, VExpVisitor> VExpImplies;
    typedef expr::BoolExpressionNot<Environment, VExpVisitor>     VExpNot;
    class VExpVar;

    typedef boost::shared_ptr<Environment> environment_ptr;

    typedef expr::Ptrs<Environment, VExpVisitor>::boolexpression_ptr  varexpression_ptr;
    typedef expr::Ptrs<Environment, VExpVisitor>::boolexp_const_ptr   vexp_const_ptr;
    typedef expr::Ptrs<Environment, VExpVisitor>::boolexp_atom_ptr    vexp_atom_ptr;
    typedef expr::Ptrs<Environment, VExpVisitor>::boolexp_and_ptr     vexp_and_ptr;
    typedef expr::Ptrs<Environment, VExpVisitor>::boolexp_or_ptr      vexp_or_ptr;
    typedef expr::Ptrs<Environment, VExpVisitor>::boolexp_implies_ptr vexp_implies_ptr;
    typedef expr::Ptrs<Environment, VExpVisitor>::boolexp_not_ptr     vexp_not_ptr;

    typedef boost::shared_ptr<VExpVar> vexp_var_ptr;

    class Environment {
        // look up ("var", mask) in masks, then val of var is env_hash & mask
        std::map<std::string, int> masks;
        int hash;

        public:
            Environment() { hash = 0; }
            Environment(const std::vector<std::string>& variables);
            Environment(const std::map<std::string, bool>& values);

            void insert_var(std::string const& var, bool val = false);

            bool get_value(std::string const& var) const;
            void set_value(std::string const& var, bool val);

            int get_hash() {
                return hash;
            }

            void restore_from_hash(int new_hash) {
                hash = new_hash;
            }

            // destroys current environment valuation
            // calls f(Envrionment&) for all possible vals
            // (note, you can update this environment)
            // (it will be destroyed before the next call)
            // (if there are no vars in the env, you get one call (the null
            // assignment i guess)
            template <typename Func>
            void forall_vals(Func f) {
                int n = masks.size();
                int local_hash = 0;
                int pwn = pow(2, n);
                for (local_hash = 0; local_hash < pwn; ++local_hash) {
                    restore_from_hash(local_hash);
                    f(*this);
                }
            }

            // calls f(var, val) over cur env
            template <typename Func>
            void env_iter(Func f) const {
                for (const std::pair<const std::string, int>& v: masks) {
                    f(v.first, get_value(v.first));
                }
            }

            void set_from(Environment const& env);

            void to_stream(std::ostream& output) const;
            friend std::ostream& operator<<(std::ostream& output, Environment const& e) {
                e.to_stream(output);
                return output;
            }
    };


    class VExpVisitor {
        public:
            virtual void visit(VExpConst& b) = 0;
            virtual void visit(VExpAnd& b) = 0;
            virtual void visit(VExpOr& b) = 0;
            virtual void visit(VExpImplies& b) = 0;
            virtual void visit(VExpNot& b) = 0;
            virtual void visit(VExpVar& b) = 0;
    };


    class VExpVar : public VExpAtom {
        bool negated;
        std::string name;

        public:
            VExpVar() { }

            VExpVar(std::string const& new_name, bool new_negated = false)
                : negated(new_negated),
                  name(new_name) { }

            void set_negated(bool new_negated) {
                negated = new_negated;
            }

            bool get_negated() {
                return negated;
            }

            void set_name(std::string const& new_name) {
                name = new_name;
            }

            std::string const& get_name() {
                return name;
            }

            virtual bool evaluate(Environment const& env) const {
                bool val = env.get_value(name);
                return negated ? !val : val;
            }

            virtual void to_stream(std::ostream& output) const {
                if (negated)
                    output << "!";
                output << name;
            }

            virtual varexpression_ptr make_shared_ptr() {
                vexp_var_ptr p(this);
                return p;
            }

            virtual void accept(VExpVisitor& v) { v.visit(*this); }

            virtual varexpression_ptr normal_form(bool negative) {
                bool new_neg = negative ? !negated : negated;
                return boost::make_shared<VExpVar>(name, new_neg);
            }
    };


}

#endif

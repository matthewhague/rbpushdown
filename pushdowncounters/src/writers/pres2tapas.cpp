
#include "pres2tapas.h"

#include "../structures/epresburger.h"

#include <ostream>

using namespace std;
using namespace pres;


void Pres2Tapas::to_stream(pres_ptr& fmla, ostream& output_stream) {
    output = &output_stream;
    *output << "{ " << DUMMY << " in nat | ";
    fmla->accept(*this);
    *output << " };";
}



void Pres2Tapas::visit(EPLessThan& obj) {
    obj.get_lhs()->accept(*this);
    *output << " < ";
    obj.get_rhs()->accept(*this);
}

void Pres2Tapas::visit(EPLessThanEq& obj) {
    obj.get_lhs()->accept(*this);
    *output << " <= ";
    obj.get_rhs()->accept(*this);
}

void Pres2Tapas::visit(EPEqual& obj) {
    obj.get_lhs()->accept(*this);
    *output << " = ";
    obj.get_rhs()->accept(*this);
}

void Pres2Tapas::visit(EPImplies& obj) {
    *output << "not (";
    obj.get_lhs()->accept(*this);
    *output << ") or (";
    obj.get_rhs()->accept(*this);
    *output << ")";
}

void Pres2Tapas::visit(EPConj& obj) {
    auto clauses = obj.get_clauses();
    auto it = clauses.begin();
    while (it != clauses.end()) {
        *output << "(";
        (*it)->accept(*this);
        *output << ")";
        if (++it != clauses.end()) {
            *output << " and ";
        }
    }
}

void Pres2Tapas::visit(EPDisj& obj) {
    auto clauses = obj.get_clauses();
    auto it = clauses.begin();
    while (it != clauses.end()) {
        *output << "(";
        (*it)->accept(*this);
        *output << ")";
        if (++it != clauses.end()) {
            *output << " or ";
        }
    }
}

void Pres2Tapas::visit(EPNeg& obj) {
    *output << "not (";
    obj.get_fmla()->accept(*this);
    *output << ")";
}

void Pres2Tapas::visit(EPExists& obj) {
    *output << "exists (";
    auto vars = obj.get_vars();
    auto it = vars.begin();
    while (it != vars.end()) {
        *output << *(it);
        if (++it != vars.end()) {
            *output << ", ";
        }
    }
    *output << ") in (";
    it = vars.begin();
    while (it != vars.end()) {
        *output << "nat";
        if (++it != vars.end()) {
            *output << ", ";
        }
    }
    *output << ") (";
    obj.get_fmla()->accept(*this);
    *output << ")";
}

void Pres2Tapas::visit(EPBool& obj) {
    *output << (obj.get_value() ? "true" : "false");
}


void Pres2Tapas::visit(EPVar& obj) {
    *output << obj;
}

void Pres2Tapas::visit(EPPlus& obj) {
    auto ops = obj.get_operands();
    if (ops.size() == 0) {
        cout << "0";
    } else {
        auto it = ops.begin();
        while (it != ops.end()) {
            (*it)->accept(*this);
            if (++it != ops.end()) {
                *output << " + ";
            }
        }
    }
}

void Pres2Tapas::visit(EPConstMult& obj) {
    *output << obj.get_scalar() << "*";
    obj.get_scalee()->accept(*this);
}

void Pres2Tapas::visit(EPInteger& obj) {
    *output << obj;
}

void Pres2Tapas::visit(EPMinus& obj) {
    *output << "(";
    obj.get_lhs()->accept(*this);
    *output << ") - (";
    obj.get_rhs()->accept(*this);
    *output << ")";
}




#include <iostream>

#include "counterexpression.h"

using namespace ctrexp;
using namespace std;

void CExpCompare::to_stream(std::ostream& output) const {
    output << variable_name << " ";
    switch (op) {
        case EQ:  output << "="; break;
        case NEQ: output << "!="; break;
        case GT:  output << ">"; break;
        case LT:  output << "<"; break;
        case GTE: output << ">="; break;
        case LTE: output << "<="; break;
    }
    output << " ";
}

CompOp CExpCompare::normal_form_op(bool negative) {
    CompOp new_op = EQ;
    if (negative) {
        switch (op) {
            case EQ:  new_op = NEQ; break;
            case NEQ: new_op = EQ; break;
            case GT: new_op = LTE; break;
            case LT: new_op = GTE; break;
            case GTE: new_op = LT; break;
            case LTE: new_op = GT; break;
        }
    } else {
        new_op = op;
    }
    return new_op;
}


counterexp_ptr CExpConstCompare::normal_form(bool negative) {
    CompOp new_op = normal_form_op(negative);
    return boost::make_shared<CExpConstCompare>(get_variable_name(),
                                                new_op,
                                                value);
}

void CExpConstCompare::to_stream(std::ostream& output) const {
    output << "(";
    CExpCompare::to_stream(output);
    output << value << ")";
}

counterexp_ptr CExpFVCompare::normal_form(bool negative) {
    CompOp new_op = normal_form_op(negative);
    return boost::make_shared<CExpFVCompare>(get_variable_name(),
                                             new_op,
                                             freevariable_name);
}

void CExpFVCompare::to_stream(std::ostream& output) const {
    output << "(";
    CExpCompare::to_stream(output);
    output << freevariable_name << ")";
}



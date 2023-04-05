

#include <string>
#include <vector>

#include "epresburger.h"

using namespace std;
using namespace pres;
        
void EPresPlus::to_stream(ostream& output) const {
    if (operands.size() == 0) {
        output << "0";
    } else {
        auto it = operands.begin(); 
        while (it != operands.end()) {
            output << "(" << (*it) << ")";
            if (++it != operands.end())
                output << " + ";
        }
    }
}


void EPresExists::to_stream(ostream& output) const {
    output << "(E)";
    auto it = vars.begin(); 
    while (it != vars.end()) {
        output << *(it);
        if(++it != vars.end())
            output << ",";
        else
            output << ".";
    }
    output << fmla;
}



void EPresCompare::to_stream(std::ostream& output) const {
    output << "(" << lhs << ") ";
    switch (op) {
        case EQ: output << "=="; break; 
        case NEQ: output << "!="; break; 
        case LT: output << "<"; break; 
        case GT: output << ">"; break; 
        case LTE: output << "<="; break; 
        case GTE: output << ">="; break; 
    }
    output << " (" << rhs << ")";
}


epresburger_ptr EPresCompare::normal_form(bool negative) {
    CompOp new_op;
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
    return boost::make_shared<EPresCompare>(lhs, new_op, rhs);
}

epresburger_ptr EPresExists::normal_form(bool negative) {
    if (!negative) {
        epresburger_ptr normal_fmla = fmla->normal_form(false);
        return boost::make_shared<EPresExists>(vars, normal_fmla);
    } else {
        cerr << "EPresExists::normal_form tried to negate an existential formula -- not allowed in existential presburger!" << endl;
        exit(-1);
    }
}




#include <string>
#include <vector>

#include "epresburger.h"

using namespace std;
using namespace pres;
        
EPAssocBin::EPAssocBin(pres_ptr& lhs, pres_ptr& rhs) {
    clauses.push_back(lhs);
    clauses.push_back(rhs);
}
   
        
void EPAssocBin::to_stream(ostream& output) const {
    auto clauses = get_clauses();
    auto it = clauses.begin(); 
    while (it != clauses.end()) {
        output << "(" << (*it) << ")";
        if (++it != clauses.end())
            output << " " << get_connective() << " ";
    }
}


void EPPlus::to_stream(ostream& output) const {
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


void EPExists::to_stream(ostream& output) const {
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



int EPAssocBin::num_vars() const {
    int count = 0;
    for (auto it = clauses.begin(); it != clauses.end(); ++it) {
        count += (*it)->num_vars();
    }
    return count;
}

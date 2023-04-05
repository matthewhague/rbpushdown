
#include "variableexpression.h"

using namespace vexp;
using namespace std;

Environment::Environment(const vector<string>& variables) {
    for (string const& s : variables) {
        insert_var(s, false);
    }
}



Environment::Environment(const map<string, bool>& values_arg) {
    for (pair<string, bool> assignment : values_arg) {
        insert_var(assignment.first, assignment.second);
    }
}

void Environment::insert_var(std::string const& var, bool val) {
    auto entry = masks.find(var);
    if (entry != masks.end()) {
        cerr << "Environment::insert_var(" 
             << var << ", " << val 
             << ") tries to add an existing variable."
             << endl;
    } else {
        // 2^n
        int mask = (1<<masks.size());
        masks.insert(make_pair(var, mask));
        set_value(var, val);
    }
}



bool Environment::get_value(string const& var) const {
    auto mask = masks.find(var);
    if (mask == masks.end()) {
        cerr << "Environment::get_value(" 
             << var 
             << ") looks up a variable that doesn't exist."
             << endl;
        exit(-1);
    }
    return mask->second & hash;
}


void Environment::set_value(string const& var, bool new_val) {
    auto mask = masks.find(var);
    if (mask == masks.end()) {
        cerr << "Environment::set_value(" 
             << var << ", " << new_val 
             << ") sets a variable that doesn't exist."
             << endl;
        exit(-1);
    }

    // set new value in hash
    if (new_val) {
        hash |= mask->second;
    } else {
        hash &= ~mask->second;
    }
}




void Environment::to_stream(ostream& output) const {
    env_iter([&] (string const& var, bool val) {
        output << var << " = " << val << "\n";
    });
}

void Environment::set_from(Environment const& env) {
    env.env_iter([&] (string const& var, bool val){
        auto mask = masks.find(var);
        if (mask != masks.end()) {
            // set new value in hash
            if (val) {
                hash |= mask->second;
            } else {
                hash &= ~mask->second;
            }
        }
    });
}



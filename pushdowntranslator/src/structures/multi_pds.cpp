
#include "../tools/tools.h"

#include "multi_pds.h"

using namespace std;
using namespace pds;

void MultiPDS::to_stream(std::ostream& output) const {
    for (pds_ptr p : get_pdss()) {
        output << "PDS:\n\n" << *p;
    }
    output << "\n\nGlobal Rules:\n\n";
    for (globalrule_ptr r : get_global_rules()) {
        output << *r << "\n";
    }
}


void GlobalRule::to_stream(std::ostream& output) const {
    output << "(";
    tools::write_list(get_controls_before(), output, [&] (string const& s) {
        output << s;
    });
    output << ") ----> (";
    tools::write_list(get_controls_after(), output, [&] (string const& s) {
        output << s;
    });
    output << ")";
}


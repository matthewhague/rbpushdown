

#include <iostream>
#include <fstream>
#include <iterator>
#include <vector>

#include "../structures/pds.h"
#include "../structures/problem.h"
#include "../structures/cfg.h"
#include "../structures/epresburger.h"
#include "../translators/pds2cfg.h"
#include "../translators/cfg2pres.h"
#include "../translators/problem2pres.h"
#include "../writers/pres2tapas.h"
#include "../writers/pres2smt.h"

#include "boost/make_shared.hpp"

#include "boost/program_options.hpp"

using namespace std;
using namespace pres;

namespace po = boost::program_options;

#define HELP "help"
#define INPUT "input-file"
#define REVERSALS "reversals"
#define PRINT_PROB "showprob"
#define NO_MIN "nominimise"

int main(int argc, char *argv[]) {

    po::options_description named("Allowed Options");
    named.add_options()
        (HELP, "produce help message")
        (INPUT, po::value<string>(), "the input file (can be passed without --input-file)")
        (REVERSALS, po::value<int>(), "override the number of reversals")
        (PRINT_PROB, "print out the parsed problem")
        (NO_MIN, "don't minimise pds by compressing silent rules")
    ;
    po::positional_options_description positional; 
    positional.add(INPUT, 1);

    po::variables_map vm;
    po::store(po::command_line_parser(argc, argv)
                           .options(named)
                           .positional(positional).run(), vm);
    po::notify(vm);    

    if (vm.count(HELP)) {
        cout << named << "\n";
        return 1;
    }

    if (vm.count(INPUT)) {
        string const& name = vm[INPUT].as<string>();
        Problem p;
        if (p.parse_problem_file(name)) {
            if (vm.count(REVERSALS)) {
                p.set_reversals(vm[REVERSALS].as<int>());
            }
            if (!vm.count(NO_MIN)) {
//                cout << "Before min: ";
//                cout << p.get_pds().get_rules().size() << endl;
                p.minimise_pds();
//                cout << "After min: ";
//                cout << p.get_pds().get_rules().size() << endl;
            }
            if (vm.count(PRINT_PROB)) {
                cout << p << endl;
            }
            Problem2pres translator;
            pres_ptr translation = translator.translateProblem(p);

            Pres2SMTLIBPretty writer;
            writer.to_stream(translation, cout);
            cout << endl;
        } else {
            cerr << "Error reading problem from " << name << "." << endl;
            cerr << p << endl;
            exit(-1);
        }
    } else {
        cerr << "Please specify an input file." << endl;
        exit(-1);
    }

    return 0;
}

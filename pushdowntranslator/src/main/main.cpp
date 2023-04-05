

#include <iostream>
#include <stdio.h>
#include <sstream>
#include <map>

#include "boost/program_options.hpp"
#include "boost/shared_ptr.hpp"
#include "boost/make_shared.hpp"
#include "boost/tokenizer.hpp"

#include "../structures/pds.h"
#include "../structures/program.h"
#include "../translators/prog2pds.h"
#include "../writers/pds2prolog.h"
#include "../codeoptimisers/codeoptimiser.h"
#include "../codeoptimisers/skipremover.h"
#include "../pdsoptimisers/mpdsoptimiser.h"
#include "../pdsoptimisers/rulecompressor.h"
#include "../pdsoptimisers/pushpopcompressor.h"
#include "../pdsoptimisers/silentloopremover.h"


using namespace std;
using namespace pds;
using namespace prog;
using namespace pdswr;

namespace po = boost::program_options;

#define HELP                 "help"
#define INPUT                "input-file"
#define PRINT_PROG           "showprog"
#define PRINT_OPTIMISED      "show-optimised"
#define CODE_OPTIMISATIONS   "progopts"
#define PDS_OPTIMISATIONS    "pdsopts"
#define SKIPS                "skips"
#define COMPRESS             "compress"
#define PUSHPOP              "pushpop"
#define SILENT_LOOP          "silentloop"
#define NONE                 "none"
#define SWITCHES             "switches"
#define REVERSALS            "reversals"

string const CODE_OPTIMISATIONS_TEXT =
"the optimisation chain on source code (default is all in the following order: skips.)\n \
program optimisers are:\n \
\n \
    skips -- removes skip statements.\n \
    none  -- no optimisation\n";
string const PDS_OPTIMISATIONS_TEXT =
"the optimisation chain on constructed PDS (default is all in the following order: compress,pushpop,silentloop.)  Chain is repeated to a fixed point.\n \
pds optimisations are:\n \
\n \
    compress    -- combine successive rules into\n \
                   single rules when no behaviours\n \
                   will be lost.\n \
    pushpop     -- if a push is followed only by pops,\n \
                   replace with no push if no\n \
                   behaviours will be lost.\n \
    silentloop  -- if p a --> p a without doing\n \
                   anything else, remove rule.\n \
    none        -- no optimisation\n";

extern int yyparse();
extern int yylex_destroy();
extern int yyset_in(FILE*);
extern program_ptr program;



void set_reversals(po::variables_map const& vm, program_ptr prog) {
    if (vm.count(REVERSALS)) {
        string const& arg = vm[REVERSALS].as<string>();
        boost::escaped_list_separator<char> separator("\\",",", "\"");
        boost::tokenizer<boost::escaped_list_separator<char>> tok(arg, separator);
        auto it = tok.begin();
        auto itend = tok.end();
        while (it != itend) {
            string c = *(it++);
            if (it != itend) {
                string ns = *(it++);
                int n;
                istringstream(ns) >> n;
                prog->set_reversals(c, n);
            } else {
                cerr << "Error in " << REVERSALS << " argument: "
                     << c << " is missing a number of reversals specification."
                     << endl;
                exit(-1);
            }
        }
    }
}



template <class T>
void fill_opts(po::variables_map const& vm,
               string const& lookup,
               map<string, T> const& optimisers,
               vector<string>& opts) {
    opts.clear();
    if (vm.count(lookup)) {
        string const& arg = vm[lookup].as<string>();
        boost::escaped_list_separator<char> separator("\\",",", "\"");
        boost::tokenizer<boost::escaped_list_separator<char>> tok(arg, separator);
        for(string opt : tok) {
            if (optimisers.count(opt)) {
                opts.push_back(opt);
            } else {
                cerr << "fill_code_opts: Optimiser "
                     << opt
                     << " specified on command line does not exist."
                     << endl;
                exit(-1);
            }
        }
    } else {
        for (const pair<const string, T>& opt : optimisers) {
            opts.push_back(opt.first);
        }
    }

}


void apply_code_optimisers(program_ptr prog,
                           vector<string> const& opts,
                           map<string, codeoptimiser_ptr> const& optimisers) {
    for (string const& opt : opts) {
        auto optimiser = optimisers.find(opt);
        if (optimiser != optimisers.end()) {
            optimiser->second->optimise(prog);
        } else {
            cerr << "apply_code_optimisers: Optimiser " << opt << " not found!" << endl;
            exit(-1);
        }
    }
}

void apply_pds_optimisers(multipds_ptr mpds,
                          vector<string> const& opts,
                          map<string, mpdsoptimiser_ptr> const& optimisers) {
    //cout << "Size in: " << mpds->size() << "\n";
    bool has_changed = true;
    while (has_changed) {
        has_changed = false;
        for (string const& opt : opts) {
            auto optimiser = optimisers.find(opt);
            if (optimiser != optimisers.end()) {
                has_changed |= optimiser->second->optimise(mpds);
            } else {
                cerr << "apply_code_optimisers: Optimiser " << opt << " not found!" << endl;
                exit(-1);
            }
        }
    }
    //cout << "Size out: " << mpds->size() << "\n";
}




int main(int argc, char *argv[]) {
    po::options_description named("Allowed Options");
    named.add_options()
        (HELP, "produce help message")
        (INPUT, po::value<string>(), "the input file (can be passed without --input-file)")
        (PRINT_PROG, "print out the parsed program")
        (SWITCHES, po::value<int>(), "override the number of context switches.")
        (REVERSALS, po::value<string>(), "override the number of reversals, syntax: counter,revs,counter,revs...")
        (PRINT_OPTIMISED, "show program after code optimisations")
        (CODE_OPTIMISATIONS, po::value<string>(), CODE_OPTIMISATIONS_TEXT.c_str())
        (PDS_OPTIMISATIONS, po::value<string>(), PDS_OPTIMISATIONS_TEXT.c_str())
    ;
    po::positional_options_description positional;
    positional.add(INPUT, 1);

    po::variables_map vm;
    po::store(po::command_line_parser(argc, argv)
                           .options(named)
                           .positional(positional).run(), vm);
    po::notify(vm);


    map<string, codeoptimiser_ptr> codeoptimisers;
    codeoptimisers.insert(make_pair(NONE, boost::make_shared<NoCodeOptimiser>()));
    codeoptimisers.insert(make_pair(SKIPS, boost::make_shared<SkipRemover>()));

    map<string, mpdsoptimiser_ptr> mpdsoptimisers;
    mpdsoptimisers.insert(make_pair(NONE, boost::make_shared<NoMPDSOptimiser>()));
    mpdsoptimisers.insert(make_pair(COMPRESS, boost::make_shared<RuleCompressor>()));
    mpdsoptimisers.insert(make_pair(PUSHPOP, boost::make_shared<PushPopCompressor>()));
    mpdsoptimisers.insert(make_pair(SILENT_LOOP, boost::make_shared<SilentLoopRemover>()));

    if (vm.count(INPUT) && !vm.count(HELP)) {
        string const& name = vm[INPUT].as<string>();

        FILE *fin = fopen(name.c_str(), "r");
        yyset_in(fin);
        yyparse();
        yylex_destroy();
        fclose(fin);

        if (vm.count(SWITCHES)) {
            int switches = vm[SWITCHES].as<int>();
            program->set_context_switches(switches);
        }
        set_reversals(vm, program);

        if (vm.count(PRINT_PROG)) {
            cout << "Uncompressed Program:\n\n\n";
            cout << (*program) << endl;
        }

        vector<string> opts;
        fill_opts(vm, CODE_OPTIMISATIONS, codeoptimisers, opts);
        apply_code_optimisers(program, opts, codeoptimisers);

        if (vm.count(PRINT_OPTIMISED)) {
            cout << "Compressed Program:\n\n\n";
            cout << (*program) << endl;
        }

        prog2pds::Prog2PDS p2p;

        multipds_ptr mpds = p2p.translate(program);

        fill_opts(vm, PDS_OPTIMISATIONS, mpdsoptimisers, opts);
        apply_pds_optimisers(mpds, opts, mpdsoptimisers);

//        for (pds_ptr pds : mpds->get_pdss()) {
//            cout << "has rules " << pds->get_rules().size() << endl;
//        }

        PDS2Prolog writer;
        writer.to_stream(mpds, cout);
    } else {
        cout << named << "\n";
        return 1;
    }

    return 0;
}



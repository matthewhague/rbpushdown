

#pragma once
#ifndef __CODEOPTIMISER_H__
#define __CODEOPTIMISER_H__

#include "boost/shared_ptr.hpp"

#include "../structures/program.h"

namespace prog {

    class CodeOptimiser {
        public:
            virtual void optimise(program_ptr prog) = 0;
    };

    typedef boost::shared_ptr<CodeOptimiser> codeoptimiser_ptr;

    class NoCodeOptimiser : public CodeOptimiser {
        public:
            virtual void optimise(program_ptr prog) { 
                // do nothing 
            }
    };

}

#endif

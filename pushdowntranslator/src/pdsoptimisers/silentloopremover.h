


#pragma once
#ifndef __SILENTLOOPREMOVE_H__
#define __SILENTLOOPREMOVE_H__

#include <set>
#include <vector>

#include "../structures/pds.h"

#include "mpdsoptimiser.h"

namespace pds {

    class SilentLoopRemover : public MPDSOptimiser {
        public:
            virtual bool optimise(multipds_ptr pds);
        private:
            bool do_remove_loops(pds_ptr pds);
            bool has_non_empty_action(rule_const_ptr rule);
    };

}


#endif


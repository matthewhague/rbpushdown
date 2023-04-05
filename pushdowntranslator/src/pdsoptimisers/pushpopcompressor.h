

#pragma once
#ifndef __PUSHPOPCOMPRESSOR_H__
#define __PUSHPOPCOMPRESSOR_H__

#include <set>
#include <vector>

#include "../structures/pds.h"
#include "../structures/counterexpression.h"

#include "mpdsoptimiser.h"

namespace pds {

    class PushPopCompressor : public MPDSOptimiser {
        public:
            virtual bool optimise(multipds_ptr pds);

        private:
            bool do_remove_calls(pds_ptr pds, 
                                 std::set<std::string> const& removable_stacks);
            bool only_pop_from(std::string const& p, 
                               std::string const& a,
                               pds_ptr pds);
    };

}


#endif


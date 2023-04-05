
#pragma once
#ifndef _PDSOPTIMISER_H_
#define __PDSOPTIMISER_H__

#include "boost/shared_ptr.hpp"

#include "../structures/pds.h"

namespace pds {

    class PDSOptimiser {
        public:
            // return true if change
            virtual bool optimise(pds_ptr pds) = 0;
    };

    typedef boost::shared_ptr<PDSOptimiser> pdsoptimiser_ptr;

}

#endif

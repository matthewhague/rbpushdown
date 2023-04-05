
#pragma once
#ifndef __PDSWRITER_H__
#define __PDSWRITER_H__


#include <ostream>

#include "../structures/multi_pds.h"

namespace pdswr {

    class PDSWriter {
        public:
            virtual void to_stream(pds::multipds_ptr mpds, std::ostream& output) = 0;

    };

}


#endif

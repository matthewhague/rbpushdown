
#pragma once

#include <ostream>

#include "../structures/epresburger.h"

class Pres2Stream {
    public:
        virtual void to_stream(pres::pres_ptr& fmla, std::ostream& output) = 0;

};

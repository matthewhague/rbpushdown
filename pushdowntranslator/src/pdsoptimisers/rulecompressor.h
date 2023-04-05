
// Rule compression aims to take rules
//
//      p a -- x --> q b
//      q b -- y --> r w
//
// and replace them with
//
//      p a -- x, y --> r w
//
// by identifying "q b" as a removable head.
//
// To be correct we make the following exceptions to removable heads:
//
//      + if b is a return location, i.e. turns up in a rule
//          
//          p a -- x --> q c ... b ... 
//
//        and
//           
//      + if b is a context switch location, i.e. b shows up in a rule
//
//          q b -- x --> r w, or
//          p a -- x --> q b w
//
//        where q is a control state that may be subject to a global transition,
//        i.e.,
//
//          (..., q, ...) -----> (..., q', ...) .
//
//      + appears before or after a counter update, or before a counter test.
//
// That is, we don't want to remove the rule q b -- y --> r w if q b might be
// reached from elsewhere, nor do we want to jump over a location that might
// have been part of a global synchronisation.


#pragma once
#ifndef __RULECOMPRESSOR_H__
#define __RULECOMPRESSOR_H__

#include <set>
#include <vector>

#include "../structures/pds.h"
#include "../structures/counterexpression.h"

#include "mpdsoptimiser.h"

namespace pds {

    class RuleCompressor : public MPDSOptimiser {
        bool remove_calls;

        public:
            virtual bool optimise(multipds_ptr pds);

        private:
            bool compress_pds(pds_ptr pds, 
                              std::set<std::string> const& removable_stacks);
            bool do_remove_head(std::string const& q, 
                                std::string const& b,
                                pds_ptr pds);
            bool has_loop_from(std::string const& q, 
                               std::string const& b,
                               pds_ptr pds);
            bool has_push_to(std::string const& q, 
                             std::string const& b,
                             pds_ptr pds);

    };

}


#endif


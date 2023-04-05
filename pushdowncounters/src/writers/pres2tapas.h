
#include "pres2stream.h"

#include "../structures/epresburger.h"

#include <ostream>

#define DUMMY "dummy"

class Pres2Tapas : public Pres2Stream, 
                   public pres::EPresburgerVisitor, 
                   public pres::EPValVisitor { 

    std::ostream* output;
    pres::pres_ptr fmla;

    public:
        virtual void to_stream(pres::pres_ptr& fmla, std::ostream& output);

        virtual void visit(pres::EPLessThan& obj);
        virtual void visit(pres::EPLessThanEq& obj);
        virtual void visit(pres::EPEqual& obj);
        virtual void visit(pres::EPImplies& obj);
        virtual void visit(pres::EPConj& obj);
        virtual void visit(pres::EPDisj& obj);
        virtual void visit(pres::EPNeg& obj);
        virtual void visit(pres::EPExists& obj);
        virtual void visit(pres::EPBool& obj);

        virtual void visit(pres::EPVar& obj);
        virtual void visit(pres::EPPlus& obj);
        virtual void visit(pres::EPConstMult& obj);
        virtual void visit(pres::EPInteger& obj);
        virtual void visit(pres::EPMinus& obj);
};

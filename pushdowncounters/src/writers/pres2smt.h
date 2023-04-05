


#include "pres2stream.h"

#include "../structures/epresburger.h"

#include <ostream>
#include <map>
#include <string>
#include <set>


class Pres2SMT: public Pres2Stream, 
                public pres::EPresburgerVisitor, 
                public pres::EPValVisitor { 

    pres::pres_ptr fmla;
    map<pres::pres_var_ptr, string> unique_names;

    protected:
        std::ostream* output;

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

    protected:
        virtual void write_fmla_command() = 0;
        virtual void write_indent() { *output << " "; }
        virtual void increase_indent(int depth) { }
        virtual void decrease_indent(int depth) { }

    private:
        bool get_unique_vars(pres::pres_ptr fmla, std::set<string>& vars);
        void do_assoc_bin(string const& connective, string const& def, pres::EPAssocBin const& obj);
        void do_compare(pres::EPCompare const& obj, string const& op);
};


class Pres2OpenSMT: public Pres2SMT {
    protected:
        virtual void write_fmla_command() { *output << ":formula 1\n:assumption"; }
};


class Pres2SMTLIB: public Pres2SMT {
    protected:
        virtual void write_fmla_command() { *output << ":formula"; }
};

class Pres2SMTPretty: public Pres2SMT {
    int indent;

    public:
        Pres2SMTPretty() : indent(0) { }

    protected:
        virtual void write_fmla_command() = 0;
        virtual void increase_indent(int amount) { indent += amount; }
        virtual void decrease_indent(int amount) { indent -= amount; }
        virtual void write_indent(); 
};

class Pres2SMTLIBPretty : public Pres2SMTPretty {
    protected:
        virtual void write_fmla_command() { *output << ":formula"; }
};

class Pres2OpenSMTPretty : public Pres2SMTPretty {
    protected:
        virtual void write_fmla_command() { *output << ":formula 1\n:assumption"; }
};

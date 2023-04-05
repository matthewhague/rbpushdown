

#include <iostream>

namespace tools {

    template <class Container, typename Func>
    void write_list_sep(Container const& c, 
                        std::string sep, 
                        std::ostream& output, 
                        Func f) {
        auto it = c.begin();
        while (it != c.end()) {
            f(*it);
            if (++it != c.end())
                output << sep;
        }
    }

    template <class Container, typename Func>
    void write_list(Container const& c, 
                    std::ostream& output, 
                    Func f) {
        write_list_sep(c, ",", output, f);
    }


}

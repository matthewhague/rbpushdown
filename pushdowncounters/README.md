
To compile you need:

    cmake: http://www.cmake.org/
    boost: http://www.boost.org/

Both should be available direct from your distribution.

Then, from the project root directory, run:

    cmake .
    make
    ./bin/main/pushdown_counters

Note the "." after cmake.

Errors are likely, since it's only tested on my work machine, and i'm not sure
how well cmake will do at finding boost when installed on a mac, but hey...

Let me know of any errors.

Matt

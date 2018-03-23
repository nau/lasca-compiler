# Unpleasant Log of Lasca Development

These are mostly my rants on things in the industry I found crazy. 
Legacy has a overwhelming power.

# 19/02/2018 Today I learned how to print 64-bit ints in C on both Mac and Linux.

This code works fine on MacOs, 

    printf("%lli", code);
    
but gives this warning on Linux:

    warning: format ‘%lli’ expects argument of type ‘long long int’,
    but argument 2 has type ‘int64_t {aka long int}’ [-Wformat=]

Apparently the right way to print int64_t in printf/snprintf family functions is this:

    #define __STDC_FORMAT_MACROS
    #include <inttypes.h>

    uint64_t i;
    printf("%" PRId64 "\n", i);

Sigh.

# Unpleasant Log of Lasca Development

These are mostly my rants on things in the industry I found crazy. 
Legacy has a overwhelming power.

## 19/02/2018 Today I learned how to print 64-bit ints in C on both Mac and Linux.

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

## 17/08/2018 State of Unicode support in programming languages

[SO overview](https://stackoverflow.com/questions/1036585/unicode-support-in-various-programming-languages)

Everything is very, very sad.
Only few modern languages use UTF-8 out of the box: Rust, Julia, and Go.
Others do various hacks or use UTF-16, which is even worse.
Legacy has an overwhelming power.

Sigh.

http://utf8everywhere.org/
[UTF-16 Considered Harmfull](https://softwareengineering.stackexchange.com/questions/102205/should-utf-16-be-considered-harmful)

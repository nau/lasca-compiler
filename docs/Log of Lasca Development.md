# Log of Lasca Development

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

## 08/10/2018 Default Hash Function and Hash table

Choosing a hash function is a crucial choice.

Most languages/platforms changed their hashing functions to something more secure. Many chose SipHash:

- Python (starting in version 3.4)
- Ruby
- Rust

In Lasca we'll use SipHash

[SipHash: a fast short-input PRF](https://131002.net/siphash/siphash.pdf)

[Which hashing algorithm is best for uniqueness and speed?](https://softwareengineering.stackexchange.com/questions/49550/which-hashing-algorithm-is-best-for-uniqueness-and-speed)

Java's default String hash algorithm is aweful

    s[0]*31^(n-1) + s[1]*31^(n-2) + ... + s[n-1]

It's easy to find collisions

Haskell uses [FNV1](http://isthe.com/chongo/tech/comp/fnv/) in [hashable](http://hackage.haskell.org/package/hashable-1.2.6.1/docs/src/Data-Hashable-Class.html#line-627)

Links
From this paper
https://bigdata.uni-saarland.de/publications/p249-richter.pdf

we consider
Mult as the best candidate to be used in practice when
quality results on high throughputs is desired, but at the cost of
a high variance across data distributions

We can conclude that RH provides a very interesting
trade-off: for a small penalty (often within 1-5%) in peak
performance on the best of cases (all lookups successful), RH
significantly improves on the worst-case over LP in general, up
to more than a factor 4.
Across the whole set of experiments, RH is always among
the top performers, and even the best method for most cases.
This observation holds for all data set sizes we tested.

As a conclusion,
in a write-heavy workload, quadratic probing looks as the best
option in general.

Our overall conclusion is that AoS outperforms
SoA by a larger margin than the other way around. Inside
caches (not shown), both methods are comparable in terms of
lookup performance, with AoS performing slightly better. When
using SIMD, SoA has an edge over AoS — at least on current
hardware — because keys are already densely packed.

https://github.com/leo-yuriev/t1ha
http://cyan4973.github.io/xxHash/
https://accidentallyquadratic.tumblr.com/post/153545455987/rust-hash-iteration-reinsertion
https://github.com/google/highwayhash/issues/28
https://medium.freecodecamp.org/hash-table-attack-8e4371fc5261
https://rcoh.me/posts/hash-map-analysis/
http://codecapsule.com/2013/11/17/robin-hood-hashing-backward-shift-deletion/
https://probablydance.com/2017/02/26/i-wrote-the-fastest-hashtable/

### Design Decisions based on research

Open addressing with Robin Hood Probing and backward shift deletion
Hash function [xxHash 64](http://cyan4973.github.io/xxHash/)
Grow either by prime numbers or powers of 2
Load factor threshold ≈ 0.6-0.7
Collision count threshold ≈ log₂(n)

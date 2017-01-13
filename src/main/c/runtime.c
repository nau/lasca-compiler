#include <gc.h>

// At the moment we rely on the conservative
// mode of Boehm GC as our garbage collector.

void lasca_init() {
    GC_init();
    puts("Init Lasca 0.0.0.1 runtime :)");
}

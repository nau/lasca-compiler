#include <stdio.h>
#include <unistd.h>
#include <gc.h>

struct type_info {
  char type;
  void* value;
};


void lasca_init() {
    GC_init();
    puts("Init Lasca 0.0.0.1 runtime :)");
}

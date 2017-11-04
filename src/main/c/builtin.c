#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include <gc.h>

#include "lasca.h"

/* ================== IO ================== */
String* unsafeString(Box* b) {
    return b->value.ptr;
}


void* putInt(int64_t c) {
    printf("%lld\n", c);
    fflush(stdout);
    return 0;
}

void* putchard(double X) {
    printf("%12.9lf\n", X);
    fflush(stdout);
    return 0;
}

void * runtimePutchar(Box* ch) {
    char c = (char) unboxInt(ch);
    putchar(c);
    fflush(stdout);
    return 0;
}

Box* println(Box* val) {
    String * str = unbox(STRING, val);
    printf("%s\n", str->bytes);
    return &UNIT_SINGLETON;
}

int64_t runtimeCompare(Box* lhs, Box* rhs) {
    if (lhs->type != rhs->type) {
        printf("AAAA!!! Type mismatch! lhs = %s, rhs = %s\n", typeIdToName(lhs->type), typeIdToName(rhs->type));
        exit(1);
    }
    int64_t result = 0;
    switch (lhs->type) {
        case BOOL:
        case INT:
        case DOUBLE: { result = // FIXME it's wrong for double
                lhs->value.num < rhs->value.num ? -1 :
                lhs->value.num == rhs->value.num ? 0 : 1;
            break;
        }
        case STRING: result = strcmp(unsafeString(lhs)->bytes, unsafeString(rhs)->bytes); // TODO do proper unicode stuff
    }
    result = result < 0 ? (int64_t) -1 : result == 0 ? 0 : 1;
    return result;
}

Box* arrayApply(Box* arrayValue, int64_t index) {
    Array* array = unbox(ARRAY, arrayValue);
    assert(array->length > index);
    return array->data[index];
}

int64_t arrayLength(Box* arrayValue) {
    Array* array = unbox(ARRAY, arrayValue);
    return array->length;
}

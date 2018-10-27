#define __STDC_FORMAT_MACROS
#include <inttypes.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include <gc.h>
#include <ffi.h>
#include <utf8proc.h>

#include "lasca.h"

#define STR(s) {.type = &String_LaType, .length = sizeof(s) - 1, .bytes = s}

// Primitive Types
const LaType Unknown_LaType = { .name = "Unknown" };
const LaType Unit_LaType    = { .name = "Unit" };
const LaType Bool_LaType    = { .name = "Bool" };
const LaType Byte_LaType    = { .name = "Byte" };
const LaType Int16_LaType   = { .name = "Int16" };
const LaType Int32_LaType   = { .name = "Int32" };
const LaType Int_LaType     = { .name = "Int" };
const LaType Float_LaType   = { .name = "Float" };
const LaType String_LaType  = { .name = "String" };
const LaType Closure_LaType = { .name = "Closure" };
const LaType Array_LaType   = { .name = "Array" };
const LaType ByteArray_LaType     = { .name = "ByteArray" };
const LaType _VAR     = { .name = "Var" };
const LaType _FILE_HANDLE   = { .name = "FileHandle" };
const LaType _PATTERN = { .name = "Pattern" };
const LaType _OPTION =  { .name = "Option" };
const LaType* UNKNOWN = &Unknown_LaType;
const LaType* UNIT    = &Unit_LaType;
const LaType* BOOL    = &Bool_LaType;
const LaType* BYTE    = &Byte_LaType;
const LaType* INT16   = &Int16_LaType;
const LaType* INT32   = &Int32_LaType;
const LaType* INT     = &Int_LaType;
const LaType* FLOAT64 = &Float_LaType;
const LaType* STRING  = &String_LaType;
const LaType* CLOSURE = &Closure_LaType;
const LaType* ARRAY   = &Array_LaType;
const LaType* VAR     = &_VAR;
const LaType* BYTEARRAY   = &ByteArray_LaType;
const LaType* FILE_HANDLE = &_FILE_HANDLE;
const LaType* PATTERN = &_PATTERN;
const LaType* OPTION  = &_OPTION;

Bool TRUE_SINGLETON = {
    .type = &Bool_LaType,
    .num = 1
};

Bool FALSE_SINGLETON = {
    .type = &Bool_LaType,
    .num = 0
};

Unit UNIT_SINGLETON = {
    .type = &Unit_LaType
};
String EMPTY_STRING = STR("\00");
String* UNIT_STRING;
Byte BYTE_ARRAY[256];
Int INT_ARRAY[100];
Float64 FLOAT64_ZERO = {
    .type = &Float_LaType,
    .num = 0.0
};

DataValue NONE = {
    .type = &_OPTION,
    .tag = 0,
    .values = {}
};
Environment ENV;
Runtime* RUNTIME;

bool eqTypes(const LaType* lhs, const LaType* rhs) {
    return lhs == rhs || strcmp(lhs->name, rhs->name) == 0;
}

Option* some(Box* value) {
    assert(value != NULL);
    DataValue* dv = gcMalloc(sizeof(DataValue) + sizeof(Box*));
    dv->type = OPTION;
    dv->tag = 1;
    dv->values[0] = value;
    return dv;
}

static uint64_t Lasca_Allocated = 0;
static uint64_t Lasca_Nr_gcMalloc = 0;

void *gcMalloc(size_t s) {
    Lasca_Allocated += s;
    Lasca_Nr_gcMalloc++;
    return GC_malloc(s);
}

void *gcMallocAtomic(size_t s) {
    Lasca_Allocated += s;
    Lasca_Nr_gcMalloc++;
    return GC_malloc_atomic(s);
}

void *gcRealloc(void* old, size_t s) {
    return GC_realloc(old, s);
}


const char * __attribute__ ((const)) typeIdToName(const LaType* typeId) {
    return typeId->name;
}

/* =============== Boxing ================== */

Box *box(const LaType* type_id, void *value) {
    Box* ti = (Box*) value;
    ti->type = type_id;
    return ti;
}

Bool* __attribute__ ((pure)) boxBool(int8_t i) {
    switch (i) {
        case 0: return &FALSE_SINGLETON; break;
        default: return &TRUE_SINGLETON; break;
    }
}

Unknown* __attribute__ ((pure)) boxError(String *name) {
    Unknown* value = gcMalloc(sizeof(Unknown));
    value->type = UNKNOWN;
    value->error = name;
    return value;
}

Byte* __attribute__ ((pure)) boxByte(int8_t i) {
    return &BYTE_ARRAY[i + 128];
}

Int* boxInt(int64_t i) {
    if (i >= 0 && i < 100) return &INT_ARRAY[i];
    else {
        Int* ti = gcMallocAtomic(sizeof(Int));
        ti->type = INT;
        ti->num = i;
        return ti;
    }
}

Int16* boxInt16(int16_t i) {
    Int16* ti = gcMallocAtomic(sizeof(Int16));
    ti->type = INT16;
    ti->num = i;
    return ti;
}

Int32* boxInt32(int32_t i) {
    Int32* ti = gcMallocAtomic(sizeof(Int32));
    ti->type = INT32;
    ti->num = i;
    return ti;
}

Float64* __attribute__ ((pure)) boxFloat64(double i) {
    if (i == 0.0) return &FLOAT64_ZERO;
    Float64* ti = gcMallocAtomic(sizeof(Float64));
    ti->type = FLOAT64;
    ti->num = i;
    return ti;
}

Closure* boxClosure(int64_t idx, int64_t argc, Box** args) {
    Closure* cl = gcMalloc(sizeof(Closure));
  //  printf("boxClosure(%d, %d, %p)\n", idx, argc, args);
  //  fflush(stdout);
    cl->type = CLOSURE;
    cl->funcIdx = idx;
    cl->argc = argc;
    cl->argv = args;
  //  printf("Enclose %d, argc = %d, args[0].type = %d, args[1].type = %d\n", idx, argc, args[0]->type, args[1]->type);
  //  fflush(stdout);
    return cl;
}

void * unbox(const LaType* expected, const Box* ti) {
  //  printf("unbox(%d, %d) ", ti->type, (int64_t) ti->value);
    /* In most cases we can use pointer comparison,
       but when we use Lasca defined type in C code, we also define a LaType
       and we need to compare actual qualified type names.
       TODO/FIXME: think how to make it better, now it's O(typename_length), show be O(1)
       Likely, not an issue anyway.
    */
    if (eqTypes(ti->type, expected)) {
        return (void*) ti;
    } else if (eqTypes(ti->type, UNKNOWN)) {
        String *name = ((Unknown *) ti)->error;
        printf("AAAA!!! Undefined identifier %s\n", name->bytes);
        exit(1);
    } else {
        printf("AAAA!!! Expected %s but got %s %p != %p\n", typeIdToName(expected), typeIdToName(ti->type), expected, ti->type);
        exit(1);
    }
}

/* ==================== Runtime Ops ============== */

Box* writeVar(DataValue* var, Box* value) {
    assert(eqTypes(var->type, VAR));
    Box* oldValue = var->values[0];
    var->values[0] = value;
    return oldValue;
}

static int64_t isBuiltinType(const Box* v) {
    const LaType* t = v->type;
    return eqTypes(t, UNIT) || eqTypes(t, BOOL) || eqTypes(t, BYTE)
      || eqTypes(t, INT) || eqTypes(t, INT16) || eqTypes(t, INT32) || eqTypes(t, FLOAT64)
      || eqTypes(t, STRING) || eqTypes(t, CLOSURE) || eqTypes(t, ARRAY) || eqTypes(t, BYTEARRAY);
}

static int64_t isUserType(const Box* v) {
    return !isBuiltinType(v);
}

#define DO_OP(op) if (eqTypes(lhs->type, INT)) { result = (Box*) boxInt(asInt(lhs)->num op asInt(rhs)->num); } \
                  else if (eqTypes(lhs->type, BYTE)) { result = (Box*) boxByte(asByte(lhs)->num op asByte(rhs)->num); } \
                  else if (eqTypes(lhs->type, INT32)) { result = (Box*) boxInt32(asInt32(lhs)->num op asInt32(rhs)->num); } \
                  else if (eqTypes(lhs->type, INT16)) { result = (Box*) boxInt16(asInt16(lhs)->num op asInt16(rhs)->num); } \
                  else if (eqTypes(lhs->type, FLOAT64)) { result = (Box*) boxFloat64(asFloat(lhs)->num op asFloat(rhs)->num); } \
                  else { \
                        printf("AAAA!!! Type mismatch! Expected Int or Float for op but got %s\n", typeIdToName(lhs->type)); exit(1); }

Box* __attribute__ ((pure)) runtimeBinOp(int64_t code, Box* lhs, Box* rhs) {
    if (!eqTypes(lhs->type, rhs->type)) {
        printf("AAAA!!! Type mismatch! lhs = %s, rhs = %s\n", typeIdToName(lhs->type), typeIdToName(rhs->type));
        exit(1);
    }

    Box* result = NULL;

    if (code == ADD) { DO_OP(+); }
    else if (code == SUB) { DO_OP(-); }
    else if (code == MUL) {DO_OP(*);}
    else if (code == DIV) {DO_OP(/);}
    else {
        int res = runtimeCompare(lhs, rhs);
        bool b = (code == EQ && res == 0) || (code == NE && res != 0) ||
                 (code == LT && res == -1) || (code == LE && res != 1) ||
                 (code == GE && res != -1) || (code == GT && res == 1);
        result = (Box*) boxBool(b);
    }
    return result;
}

Box* __attribute__ ((pure)) runtimeUnaryOp(int64_t code, Box* expr) {
    Box* result = NULL;
    switch (code) {
        case 1:
            if (eqTypes(expr->type, INT)) {
                result = (Box*) boxInt(-asInt(expr)->num);
            } else if (eqTypes(expr->type, BYTE)) {
                result = (Box*) boxByte(-asByte(expr)->num);
            } else if (eqTypes(expr->type, INT32)) {
                result = (Box*) boxInt32(-asInt32(expr)->num);
            } else if (eqTypes(expr->type, INT16)) {
                result = (Box*) boxInt16(-asInt16(expr)->num);
            } else if (eqTypes(expr->type, FLOAT64)) {
                result = (Box*) boxFloat64(-asFloat(expr)->num);
            } else {
                printf("AAAA!!! Type mismatch! Expected Int or Float for op but got %s\n", typeIdToName(expr->type));
                exit(1);
            }
            break;
        default:
            printf("AAAA!!! Unsupported unary operation %"PRId64, code);
            exit(1);
    }
    return result;
}

String UNIMPLEMENTED_SELECT = {
    .length = 20,
    .bytes = "Unimplemented select"
};

Box* runtimeApply(Box* val, int64_t argc, Box* argv[], Position pos) {
    Functions* fs = RUNTIME->functions;
    Closure *closure = unbox(CLOSURE, val);
    if (closure->funcIdx >= fs->size) {
        printf("AAAA!!! No such function with id %"PRId64", max id is %"PRId64" at line: %"PRId64"\n", (int64_t) closure->funcIdx, fs->size, pos.line);
        exit(1);
    }
    Function f = fs->functions[closure->funcIdx];
    if (f.arity != argc + closure->argc) {
        printf("AAAA!!! Function %s takes %"PRId64" params, but passed %"PRId64" enclosed params and %"PRId64" params instead at line: %"PRId64"\n",
            f.name->bytes, f.arity, closure->argc, argc, pos.line);
        exit(1);
    }

    ffi_cif cif;
    ffi_type *args[f.arity];
    void *values[f.arity];
    Box* rc;

    for (int i = 0; i < f.arity; i++) {
        args[i] = &ffi_type_pointer;
        values[i] = (i < closure->argc) ? &closure->argv[i] : &argv[i - closure->argc];
    }
    if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, f.arity, &ffi_type_pointer, args) != FFI_OK) {
    		printf("AAAA!!! Function %s ffi_prep_cif call failed\n", f.name->bytes);
        exit(1);
    }
    ffi_call(&cif, f.funcPtr, &rc, values);
    return rc;
}

Data* findDataType(const LaType* type) {
    Types* types = RUNTIME->types;
    for (int i = 0; i < types->size; i++) {
        if (eqTypes(types->data[i]->type, type)) return types->data[i];
    }
    printf("AAAA! Couldn't find type %s", type->name);
    exit(1);
}

Box* __attribute__ ((pure)) runtimeSelect(Box* tree, Box* ident, Position pos) {
    Functions* fs = RUNTIME->functions;

//    printf("isUserType %s %p %p\n", tree->type->name, tree->type, &Unknown_LaType);
    if (isUserType(tree)) {

        DataValue* dataValue = asDataValue(tree);
        // if rhs is not a local ident, nor a function, try to find this field in lhs data structure
        if (eqTypes(ident->type, UNKNOWN)) {
            String* name = ((Unknown*)ident)->error; // should be identifier name
//            printf("Ident name %s\n", name->bytes);
            Data* data = findDataType(tree->type); // find struct in global array of structs
//            printf("Found data type %s %s, tag %"PRId64"\n", data->name->bytes, tree->type->name, dataValue->tag);
            Struct* constr = data->constructors[dataValue->tag];
            int64_t numFields = constr->numFields;
            for (int64_t i = 0; i < numFields; i++) {
                String* field = constr->fields[i];
        //        printf("Check field %d %s\n", field->length, field->bytes);
                if (field->length == name->length && strncmp(field->bytes, name->bytes, name->length) == 0) {
                    Box* value = dataValue->values[i];
//                    printf("Found value %s at index %"PRId64"\n", value->type->name, i);
          //          println(toString(value));
                    return value;
                }
            }
            printf("Couldn't find field %s at line: %"PRId64"\n", name->bytes, pos.line);
        } else if (eqTypes(ident->type, CLOSURE)) {
              // FIXME fix for closure?  check arity?
              Closure* f = asClosure(ident);
              assert(fs->functions[f->funcIdx].arity == 1);
              return runtimeApply(ident, 1, &tree, pos);
        }
    } else if (eqTypes(ident->type, CLOSURE)) {
        // FIXME fix for closure?  check arity?
        Closure* f = asClosure(ident);
        assert(fs->functions[f->funcIdx].arity == 1);
        return runtimeApply(ident, 1, &tree, pos);
    }
    return (Box*) boxError(&UNIMPLEMENTED_SELECT);
}

int8_t runtimeIsConstr(Box* value, Box* constrName) {
    if (isUserType(value)) {
        String* name = unbox(STRING, constrName);
        Data* data = findDataType(value->type);
        DataValue* dv = asDataValue(value);
        String* realConstrName = data->constructors[dv->tag]->name;
        if (strncmp(realConstrName->bytes, name->bytes, fmin(realConstrName->length, name->length)) == 0)
            return true;
    }
    return false;
}

int8_t runtimeCheckTag(Box* value, int64_t tag) {
    DataValue* dv = asDataValue(value);
    return dv->tag == tag;
}

/* =================== Arrays ================= */


Array* createArray(size_t size) {
    Array * array = gcMalloc(sizeof(Array) + sizeof(Box*) * size);
    array->type = ARRAY;
    array->length = size;
    return array;
}

Box* boxArray(size_t size, ...) {
    va_list argp;
    Array * array = createArray(size);
    va_start (argp, size);
    for (int64_t i = 0; i < size; i++) {
        Box* arg = va_arg(argp, Box*);
        array->data[i] = arg;
    }
    va_end (argp);                  /* Clean up. */
    return box(ARRAY, array);
}

String* __attribute__ ((pure)) makeString(const char * str) {
    size_t len = strlen(str);
    String* val = gcMalloc(sizeof(String) + len + 1);  // null terminated
    val->type = STRING;
    val->length = len;
    strncpy(val->bytes, str, len);
    return val;
}

String* joinValues(int size, Box* values[], char* start, char* end) {
    String* strings[size];
    int startLen = strlen(start);
    int endLen = strlen(end);
    int resultSize = startLen + endLen + 1 + 2 * size;

    for (int i = 0; i < size; i++) {
        Box* elem = values[i];
        String* value = toString(elem);
        resultSize += value->length;
        strings[i] = value;
    }

    char * result = malloc(resultSize);
    strcpy(result, start);
    for (int i = 0; i < size; i++) {
        String* value = strings[i];
        strcat(result, value->bytes);
        if (i + 1 < size) strcat(result, ", ");
    }
    strcat(result, end);
    String* string = makeString(result);
    free(result);
    return string;
}

String* __attribute__ ((pure)) arrayToString(const Box* arrayValue)  {
    Array* array = unbox(ARRAY, arrayValue);
    if (array->length == 0) {
        return makeString("[]");
    } else {
        return joinValues(array->length, array->data, "[", "]");
    }
}

String* typeOf(Box* value) {
    return makeString(value->type->name);
}

String* __attribute__ ((pure)) byteArrayToString(const Box* arrayValue)  {
    String* array = unbox(BYTEARRAY, arrayValue);
    if (array->length == 0) {
        return makeString("[]");
    } else {
        int len = 6 * array->length + 2 + 1; // max (4 + 2 (separator)) symbols per byte + [] + 0
        String* res = gcMalloc(sizeof(String) + len);
        res->type = STRING;
        strcpy(res->bytes, "[");
        char buf[7];
        int curPos = 1;
        for (int i = 0; i < array->length; i++) {
            int l = (i < array->length - 1) ?
              snprintf(buf, 7, "%"PRId8", ", array->bytes[i]) : snprintf(buf, 5, "%"PRId8, array->bytes[i]);
            strcpy(res->bytes + curPos, buf);
            curPos += l;
        }
        strcpy(res->bytes + curPos, "]");
        res->length = curPos + 1;
        return res;
    }
}

bool isNull(Box* value) {
    return value == NULL;
}

Box* unsafeNull() {
    return NULL;
}


/* =============== Strings ============= */

String* __attribute__ ((pure)) toString(const Box* value) {
    char buf[100]; // 100 chars is enough for all (c)

    if (value == NULL) return makeString("<NULL>");

    const LaType* type = value->type;
    if (eqTypes(type, UNIT)) {
        return UNIT_STRING;
    } else if (eqTypes(type, BOOL)) {
        return makeString(asBool(value)->num == 0 ? "false" : "true");
    } else if (eqTypes(type, INT)) {
        snprintf(buf, 100, "%"PRId64, asInt(value)->num);
        return makeString(buf);
    } else if (eqTypes(type, INT16)) {
        snprintf(buf, 100, "%"PRId16, asInt16(value)->num);
        return makeString(buf);
    } else if (eqTypes(type, INT32)) {
        snprintf(buf, 100, "%"PRId32, asInt32(value)->num);
        return makeString(buf);
    } else if (eqTypes(type, BYTE)) {
        snprintf(buf, 100, "%"PRId8, asByte(value)->num);
        return makeString(buf);
    } else if (eqTypes(type, FLOAT64)) {
        snprintf(buf, 100, "%12.9lf", asFloat(value)->num);
        return makeString(buf);
    } else if (eqTypes(type, STRING)) {
        return asString(value);
    } else if (eqTypes(type, CLOSURE)) {
        return makeString("<func>");
    } else if (eqTypes(type, ARRAY)) {
        return arrayToString(value);
    } else if (eqTypes(type, BYTEARRAY)) {
        return byteArrayToString(value);
    } else if (eqTypes(type, VAR)) {
        DataValue* dataValue = asDataValue(value);
        return toString(dataValue->values[0]);
    } else if (eqTypes(type, UNKNOWN)) {
        String *name = ((Unknown *) value)->error;
        printf("AAAA!!! Undefined identifier in toString %s\n", name->bytes);
        exit(1);
    } else {
        if (isUserType(value)) {
            DataValue* dataValue = asDataValue(value);
            Data* metaData = findDataType(type);
            Struct* constr = metaData->constructors[dataValue->tag];
            int64_t startlen = constr->name->length + 2; // ending 0 and possibly "(" if constructor has parameters
            char start[startlen];
            snprintf(start, startlen, "%s", constr->name->bytes);
            if (constr->numFields > 0) {
                strcat(start, "(");
                return joinValues(constr->numFields, dataValue->values, start, ")");
            } else return makeString(start);
        } else {
            printf("Unsupported type %s", typeIdToName(value->type));
            exit(1);
        }
    }
}

String* concat(Box* arrayString) {
    Array* array = unbox(ARRAY, arrayString);
    String* result = &EMPTY_STRING;
    if (array->length > 0) {
        int64_t len = 0;
        for (int64_t i = 0; i < array->length; i++) {
            String* s = unbox(STRING, array->data[i]);
            len += s->length;
        }
        String* val = gcMalloc(sizeof(String) + len + 1); // +1 for null-termination
        val->type = STRING;
        // val->length is 0, because gcMalloc allocates zero-initialized memory
        // it's also zero terminated, because gcMalloc allocates zero-initialized memory
        for (int64_t i = 0; i < array->length; i++) {
            String* s = unbox(STRING, array->data[i]);
            memcpy(&val->bytes[val->length], s->bytes, s->length);
            val->length += s->length;
        }
        result = val;
    }
    return result;
}

/* ============ System ================ */

void initEnvironment(int64_t argc, char* argv[]) {
  //  int64_t len = 0;
  //  for (int64_t i = 0; i< argc; i++) len += strlen(argv[i]);
  //  char buf[len + argc*2 + 10];
  //  for (int64_t i = 0; i < argc; i++) {
  //    strcat(buf, argv[i]);
  //    strcat(buf, " ");
  //  }
  //  printf("Called with %d \n", argc);
    ENV.argc = argc;
    Array* array = createArray(argc);
    for (int64_t i = 0; i < argc; i++) {
        Box* s = (Box *) makeString(argv[i]);
        array->data[i] = s;
    }
    ENV.argv = box(ARRAY, array);
}

Box* getArgs() {
    return ENV.argv;
}

int8_t intToByte(int64_t n) {
    return (int8_t) n;
}

int64_t byteToInt(int8_t n) {
    return (int64_t) n;
}

int16_t intToInt16(int64_t n) {
    return (int16_t) n;
}

int64_t int16ToInt(int16_t n) {
    return (int64_t) n;
}

int32_t intToInt32(int64_t n) {
    return (int32_t) n;
}

int64_t int32ToInt(int32_t n) {
    return (int64_t) n;
}

int64_t toInt(Box* s) {
    String* str = unbox(STRING, s);
  //  println(s);
    char cstr[str->length + 1];
    memcpy(cstr, str->bytes, str->length); // TODO use VLA?
    cstr[str->length] = 0;
  //  printf("cstr = %s\n", cstr);
    char *ep;
    long i = strtol(cstr, &ep, 10);
    if (cstr == ep) {
        printf("Couldn't convert %s to int64_t", cstr);
        exit( EXIT_FAILURE );
    }
    return (int64_t) i;
}

void onexit() {
    struct GC_prof_stats_s stats;
    GC_get_prof_stats(&stats, sizeof(stats));

    printf("GC stats:\n");
    printf("\tFinal heap size is %lu MiB\n", (unsigned long)GC_get_heap_size() / 1024 / 1024);
    printf("\tNumber of collections %lu\n", stats.gc_no);
    printf("\tTotal allocated %zu MiB\n", GC_get_total_bytes()  / 1024 / 1024);
    printf("\tTotal allocated %"PRIu64" MiB\n", Lasca_Allocated / 1024 / 1024);
    printf("\tNumber of allocations: %"PRIu64"\n", Lasca_Nr_gcMalloc);
    printf("\tAverage alloc: %"PRIu64" bytes\n", Lasca_Allocated / Lasca_Nr_gcMalloc);
}

void initLascaRuntime(Runtime* runtime) {
    GC_init();
    GC_expand_hp(4*1024*1024);
    RUNTIME = runtime;
    UNIT_STRING = makeString("()");
    for (int i = 0; i < 256; i++) {
        BYTE_ARRAY[i].type = BYTE;
        BYTE_ARRAY[i].num = i - 128;
    }
    for (int i = 0; i < 100; i++) {
        INT_ARRAY[i].type = INT;
        INT_ARRAY[i].num = i;
    }
    if (runtime->verbose) {
        atexit(onexit);
        printf("Init Lasca 0.0.2 runtime. Enjoy :)\n# funcs = %"PRId64
               ", # structs = %"PRId64", utf8proc version %s\n",
          RUNTIME->functions->size, RUNTIME->types->size, utf8proc_version());
        for (int i = 0; i < RUNTIME->types->size; i++) {
            printf("Type %s\n", RUNTIME->types->data[i]->type->name);
        }
    }
}

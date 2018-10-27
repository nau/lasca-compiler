#ifndef LASCA_H
#define LASCA_H
#define PCRE2_CODE_UNIT_WIDTH 8
#include <pcre2.h>

// Operators
static const int64_t ADD = 10;
static const int64_t SUB = 11;                           // x - y
static const int64_t MUL = 12;
static const int64_t DIV = 13;                           // x / y
static const int64_t MOD = 14;                           // x % y

static const int64_t EQ = 42;                            // x == y
static const int64_t NE = 43;                            // x != y
static const int64_t LT = 44;                            // x < y
static const int64_t LE = 45;                            // x <= y
static const int64_t GE = 46;                            // x >= y
static const int64_t GT = 47;                            // x > y
  // Boolean unary operations
static const int64_t ZNOT = 50;                          // !x

  // Boolean binary operations
static const int64_t ZOR = 60;                           // x || y
static const int64_t ZAND = 61;                          // x && y

typedef struct {
    const char* name;
} LaType;

typedef struct {
    const LaType* type;
    void* fields[];
} Box;

typedef Box Unit;

typedef struct {
    const LaType* type;
    int8_t num;
} Byte;

typedef struct {
    const LaType* type;
    int64_t num;
} Int;

typedef struct {
    const LaType* type;
    int16_t num;
} Int16;

typedef struct {
    const LaType* type;
    int32_t num;
} Int32;

typedef Byte Bool;

typedef struct {
    const LaType* type;
    double num;
} Float64;

typedef struct {
    const LaType* type;
    int64_t length;
    char bytes[];
} String;

typedef struct {
    const LaType* type;
    int64_t funcIdx;
    int64_t argc;
    Box** argv;
} Closure;

typedef struct {
    const LaType* type;
    int64_t length;
    Box* data[];
} Array;

typedef struct {
    const LaType* type;
    int64_t tag;
    Box* values[];
} DataValue;

typedef DataValue Option;

typedef struct {
    const LaType* type;
    String* error;
} Unknown;

typedef struct {
    const LaType* type;
    pcre2_code *re;
} Pattern;

typedef struct {
    String* name;
    void * funcPtr;
    int64_t arity;
} Function;

typedef struct {
    int64_t size;
    Function functions[];
} Functions;

typedef struct {
    LaType* type;
  //  int64_t tag;   // it's not set now. Not sure we need this
    String* name;
    int64_t numFields;
    String* fields[];
} Struct;

typedef struct {
    LaType* type;
    String* name;
    int64_t numValues;
    Struct* constructors[];
} Data;

typedef struct {
    int64_t size;
    Data* data[];
} Types;

typedef struct {
    int64_t argc;
    Box* argv;
} Environment;

typedef struct {
    Functions* functions;
    Types* types;
    int8_t verbose;
} Runtime;

typedef struct {
    int64_t line;
    int64_t column;
} Position;

#define asBool(ptr) ((Bool*)ptr)
#define asByte(ptr) ((Byte*)ptr)
#define asInt(ptr) ((Int*)ptr)
#define asInt16(ptr) ((Int16*)ptr)
#define asInt32(ptr) ((Int32*)ptr)
#define asFloat(ptr) ((Float64*)ptr)
#define asString(ptr) ((String*)ptr)
#define asDataValue(ptr) ((DataValue*)ptr)
#define asClosure(ptr) ((Closure*)ptr)
#define asArray(ptr) ((Array*)ptr)
#define asByteArray(ptr) ((String*)ptr)

extern Unit UNIT_SINGLETON;
extern Bool TRUE_SINGLETON;
extern Bool FALSE_SINGLETON;
extern DataValue NONE;
// Primitive Types
extern const LaType* UNIT   ;
extern const LaType* BOOL   ;
extern const LaType* BYTE   ;
extern const LaType* INT16  ;
extern const LaType* INT32  ;
extern const LaType* INT    ;
extern const LaType* FLOAT64;
extern const LaType* STRING ;
extern const LaType* CLOSURE;
extern const LaType* ARRAY  ;
extern const LaType* BYTEARRAY;
extern const LaType* FILE_HANDLE;
extern const LaType* PATTERN;
extern const LaType* OPTION;

bool eqTypes(const LaType* lhs, const LaType* rhs);
void *gcMalloc(size_t s);
String* __attribute__ ((pure)) makeString(const char * str);
Box *box(const LaType* type_id, void *value);
Int* boxInt(int64_t i);
Int16* boxInt16(int16_t i);
Int32* boxInt32(int32_t i);
void * unbox(const LaType* expected, const Box* ti);
int64_t runtimeCompare(Box* lhs, Box* rhs);
Box* runtimeApply(Box* val, int64_t argc, Box* argv[], Position pos);
String* toString(const Box* value);
Box* println(const Box* val);
Box* boxArray(size_t size, ...);
Array* createArray(size_t size);
const char * __attribute__ ((const)) typeIdToName(const LaType* typeId);
DataValue* some(Box* value);

#endif
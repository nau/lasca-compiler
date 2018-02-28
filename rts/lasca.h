#ifndef LASCA_H
#define LASCA_H

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
    int64_t length;
    char bytes[];
} String;

typedef struct {
    const char* name;
} LaType;

typedef struct {
    const LaType* type;
    union Value {
        int64_t num;
        double dbl;
        void* ptr;
    } value;
} Box;

typedef struct {
    int64_t funcIdx;
    Box* args;  // boxed Array of boxed enclosed arguments
} Closure;

typedef struct {
    int64_t length;
    Box** data;
} Array;

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
    int64_t verbose;
} Runtime;

typedef struct {
    int64_t tag;
    Box* values[];
} DataValue;

typedef struct {
    int64_t line;
    int64_t column;
} Position;

extern Box UNIT_SINGLETON;
// Primitive Types
extern const LaType* UNIT   ;
extern const LaType* BOOL   ;
extern const LaType* INT    ;
extern const LaType* DOUBLE ;
extern const LaType* STRING ;
extern const LaType* CLOSURE;
extern const LaType* ARRAY  ;
extern const LaType* FILE_HANDLE;

void *gcMalloc(size_t s);
Box *box(const LaType* type_id, void *value);
Box * __attribute__ ((pure)) boxInt(int64_t i);
void * __attribute__ ((pure)) unbox(const LaType* expected, const Box* ti);
int64_t __attribute__ ((pure)) unboxInt(const Box* ti);
const Box* toString(const Box* value);
Box* println(const Box* val);
Box* boxArray(size_t size, ...);
Array* createArray(size_t size);
const char * __attribute__ ((const)) typeIdToName(const LaType* typeId);

#endif
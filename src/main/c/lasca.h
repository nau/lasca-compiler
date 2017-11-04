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

// Primitive Types
static const int64_t UNIT     = 0;
static const int64_t BOOL     = 1;
static const int64_t INT      = 2;
static const int64_t DOUBLE   = 3;
static const int64_t STRING   = 4;
static const int64_t CLOSURE  = 5;
static const int64_t ARRAY    = 6;

typedef struct {
    int64_t type;
    union Value {
        int64_t num;
        double dbl;
        void* ptr;
    } value;
} Box;

typedef struct {
    int64_t length;
    char bytes[];
} String;

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
    int64_t typeId;
  //  int64_t tag;   // it's not set now. Not sure we need this
    String* name;
    int64_t numFields;
    String* fields[];
} Struct;

typedef struct {
    int64_t typeId;
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

Box *box(int64_t type_id, void *value);
void * __attribute__ ((pure)) unbox(int64_t expected, Box* ti);
int64_t __attribute__ ((pure)) unboxInt(Box* ti);
Box* toString(Box* value);
Box* println(Box* val);
Box* boxArray(size_t size, ...);
const char * __attribute__ ((const)) typeIdToName(int64_t typeId);

#endif
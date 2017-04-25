#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include <gc.h>

// Operators
const int ADD = 10;
const int SUB = 11;                           // x - y
const int MUL = 12;
const int DIV = 13;                           // x / y
const int MOD = 14;                           // x % y

const int EQ = 42;                            // x == y
const int NE = 43;                            // x != y
const int LT = 44;                            // x < y
const int LE = 45;                            // x <= y
const int GE = 46;                            // x >= y
const int GT = 47;                            // x > y
  // Boolean unary operations
const int ZNOT = 50;                          // !x

  // Boolean binary operations
const int ZOR = 60;                           // x || y
const int ZAND = 61;                          // x && y

// Primitive Types
const int UNIT     = 0;
const int BOOL     = 1;
const int INT      = 2;
const int DOUBLE   = 3;
const int STRING   = 4;
const int CLOSURE  = 5;
const int ARRAY    = 6;

typedef struct {
  int type;
  union Value {
    int num;
    double dbl;
    void* ptr;
  } value;
} Box;

Box TRUE_SINGLETON = {
  .type = BOOL,
  .value.num = 1
};
Box FALSE_SINGLETON = {
  .type = BOOL,
  .value.num = 0
};
Box UNIT_SINGLETON = {
  .type = UNIT
};
Box * UNIT_STRING;
Box  INT_ARRAY[100];
Box  DOUBLE_ZERO = {
  .type = DOUBLE,
  .value.dbl = 0.0
};

typedef struct {
  int length;
  char bytes[];
} String;

typedef struct {
  int funcIdx;
  int argc;
  Box** argv;
} Closure;

typedef struct {
  int length;
  Box** data;
} Array;

typedef struct {
  String* name;
  void * funcPtr;
  int arity;
} Function;

typedef struct {
  int size;
  Function functions[];
} Functions;

typedef struct {
  int typeId;
  String* name;
  int numFields;
  String* fields[];
} Struct;

typedef struct {
  int size;
  Struct* structs[];
} Structs;

typedef struct {
  int argc;
  Box* argv;
} Environment;

typedef struct {
  Functions* functions;
  Structs* structs;
  Environment* environment;
} Runtime;

Environment ENV;
Runtime* RUNTIME;


void *gcMalloc(size_t s) {
    return GC_malloc(s);
}

void *gcMallocAtomic(size_t s) {
    return GC_malloc_atomic(s);
}

void *gcRealloc(void* old, size_t s) {
    return GC_realloc(old, s);
}

Box* toString(Box* value);
void * println(Box* val);

const char * __attribute__ ((const)) typeIdToName(int typeId) {
  switch (typeId) {
    case 0: return "Unit";
    case 1: return "Bool";
    case 2: return "Int";
    case 3: return "Double";
    case 4: return "String";
    case 5: return "<function>";
    case 6: return "Array";
    default: return "Unknown";
  }
}

/* =============== Boxing ================== */

Box *box(int type_id, void *value) {
  Box* ti = gcMalloc(sizeof(Box));
  ti->type = type_id;
  ti->value.ptr = value;
  return ti;
}

Box * __attribute__ ((pure)) boxBool(int i) {
  switch (i) {
    case 0: return &FALSE_SINGLETON; break;
    default: return &TRUE_SINGLETON; break;
  }
}

Box * __attribute__ ((pure)) boxError(String *name) {
  return box(-1, name);
}

Box * __attribute__ ((pure)) boxInt(int i) {
  if (i >= 0 && i < 100) return &INT_ARRAY[i];
  else {
    Box* ti = gcMallocAtomic(sizeof(Box));
    ti->type = INT;
    ti->value.num = i;
    return ti;
  }
}

Box * __attribute__ ((pure)) boxFloat64(double i) {
  if (i == 0.0) return &DOUBLE_ZERO;
  Box* ti = gcMallocAtomic(sizeof(Box));
  ti->type = DOUBLE;
  ti->value.dbl = i;
  return ti;
}

Box * boxClosure(int idx, int argc, Box** args) {
  Closure* cl = gcMalloc(sizeof(Closure));
//  printf("boxClosure(%d, %d, %p)\n", idx, argc, args);
//  fflush(stdout);
  cl->funcIdx = idx;
  cl->argc = argc;
  cl->argv = args;
//  printf("Enclose %d, argc = %d, args[0].type = %d, args[1].type = %d\n", idx, argc, args[0]->type, args[1]->type);
//  fflush(stdout);
  return box(CLOSURE, cl);
}

Box * __attribute__ ((pure)) boxFunc(int idx) {
//  printf("boxFunc(%d)\n", idx);
//  fflush(stdout);
  return boxClosure(idx, 0, NULL);
}

void * __attribute__ ((pure)) unbox(int expected, Box* ti) {
//  printf("unbox(%d, %d) ", ti->type, (int) ti->value);
  if (ti->type == expected) {
  	return ti->value.ptr;
  } else if (ti->type == -1) {
    String *name = (String *) ti->value.ptr;
    printf("AAAA!!! Undefined identifier %.*s\n", name->length, name->bytes);
    exit(1);
  } else {
    printf("AAAA!!! Expected %s but got %s\n", typeIdToName(expected), typeIdToName(ti->type));
    exit(1);
  }
}

int __attribute__ ((pure)) unboxInt(Box* ti) {
//  printf("unbox(%d, %d) ", ti->type, (int) ti->value);
  if (ti->type == INT) {
  	return ti->value.num;
  } else {
    printf("AAAA!!! Expected %s but got %s\n", typeIdToName(INT), typeIdToName(ti->type));
    exit(1);
  }
}

/* ==================== Runtime Ops ============== */


#define DO_OP(op) if (lhs->type == INT) { result = boxInt(left op right); } else if (lhs->type == DOUBLE) { result = boxFloat64(lhs->value.dbl op rhs->value.dbl); } else { \
                        printf("AAAA!!! Type mismatch! Expected Int or Double for op but got %s\n", typeIdToName(lhs->type)); exit(1); }

#define DO_CMP(op) switch (lhs->type){ \
                   case BOOL:    { result = boxBool (left op right); break; } \
                   case INT:     { result = boxBool (left op right); break; } \
                   case DOUBLE:  { result = boxBool (left op right); break; } \
                   default: {printf("AAAA!!! Type mismatch! Expected Bool, Int or Double but got %s\n", typeIdToName(lhs->type)); exit(1); }\
                   }
Box* __attribute__ ((pure)) runtimeBinOp(int code, Box* lhs, Box* rhs) {
  if (lhs->type != rhs->type) {
  	printf("AAAA!!! Type mismatch! lhs = %s, rhs = %s\n", typeIdToName(lhs->type), typeIdToName(rhs->type));
  	exit(1);
  }

  int left = lhs->value.num;
  int right = rhs->value.num;
  Box* result = NULL;

  switch (code) {
  case ADD: DO_OP(+); break;
  case SUB: DO_OP(-); break;
  case MUL: DO_OP(*); break;
  case DIV: DO_OP(/); break;
  case EQ:
    DO_CMP(==);
    break;
  case NE:
    DO_CMP(!=);
    break;
  case LT:
    DO_CMP(<);
    break;
  case LE:
    DO_CMP(<=);
    break;
  case GE:
      DO_CMP(>=);
      break;
  case GT:
    DO_CMP(>);
    break;
  default:
  	printf("AAAA!!! Unsupported binary operation %i", code);
    exit(1);
  }
  return result;
}

Box* arrayApply(Box* arrayValue, Box* idx) {
  Array* array = unbox(ARRAY, arrayValue);
  int index = unboxInt(idx);
  assert(array->length > index);
  return array->data[index];
}

Box* arrayLength(Box* arrayValue) {
  Array* array = unbox(ARRAY, arrayValue);
  return boxInt(array->length);
}

String s = {
  .length = 20,
  .bytes = "Unimplemented select"
};

Box* runtimeApply(Box* val, int argc, Box* argv[]) {
  Functions* fs = RUNTIME->functions;
  // Handle array(idx) call
  if (val->type == ARRAY && argc == 1 && argv[0]->type == INT) {
    return arrayApply(val, argv[0]);
  }
  Closure *closure = unbox(CLOSURE, val);
  if (closure->funcIdx >= fs->size) {
    printf("AAAA!!! No such function with id %d, max id is %d", (int) closure->funcIdx, fs->size);
    exit(1);
  }
  Function f = fs->functions[closure->funcIdx];
  if (f.arity != argc + closure->argc) {
    printf("AAAA!!! Function %.*s takes %d params, but passed %d enclosed params and %d params instead",
      f.name->length, f.name->bytes, f.arity, closure->argc, argc);
    exit(1);
  }
  // TODO: use platform ABI, like it should be.
  // Currently, it's hardcoded for applying a function with up to 6 arguments in dynamic mode
  switch (f.arity) {
    case 0: {
      void* (*funcptr)() = f.funcPtr;
      return funcptr();
    }
    case 1: {
      void* (*funcptr)(Box*) = f.funcPtr;
      return (argc == 1) ? funcptr(argv[0]) : funcptr(closure->argv[0]);
    }
    case 2: {
      void* (*funcptr)(Box*, Box*) = f.funcPtr;
      switch (argc) {
        case 0: return funcptr(closure->argv[0], closure->argv[1]);
        case 1: return funcptr(closure->argv[0], argv[0]);
        case 2: return funcptr(argv[0], argv[1]);
      }
    }
    case 3: {
      void* (*funcptr)(Box*, Box*, Box*) = f.funcPtr;
        switch (argc) {
          case 0: return funcptr(closure->argv[0], closure->argv[1], closure->argv[2]);
          case 1: return funcptr(closure->argv[0], closure->argv[1], argv[0]);
          case 2: return funcptr(closure->argv[0], argv[0], argv[1]);
          case 3: return funcptr(argv[0], argv[1], argv[2]);
        }
    }
    case 4: {
      void* (*funcptr)(Box*, Box*, Box*, Box*) = f.funcPtr;
      switch (argc) {
        case 0: return funcptr(closure->argv[0], closure->argv[1], closure->argv[2], closure->argv[3]);
        case 1: return funcptr(closure->argv[0], closure->argv[1], closure->argv[2], argv[0]);
        case 2: return funcptr(closure->argv[0], closure->argv[1], argv[0], argv[1]);
        case 3: return funcptr(closure->argv[0], argv[0], argv[1], argv[2]);
        case 4: return funcptr(argv[0], argv[1], argv[2], argv[3]);
      }
    }
    case 5: {
      void* (*funcptr)(Box*, Box*, Box*, Box*, Box*) = f.funcPtr;
      switch (argc) {
        case 0: return funcptr(closure->argv[0], closure->argv[1], closure->argv[2], closure->argv[3], closure->argv[4]);
        case 1: return funcptr(closure->argv[0], closure->argv[1], closure->argv[2], closure->argv[3], argv[0]);
        case 2: return funcptr(closure->argv[0], closure->argv[1], closure->argv[2], argv[0], argv[1]);
        case 3: return funcptr(closure->argv[0], closure->argv[1], argv[0], argv[1], argv[2]);
        case 4: return funcptr(closure->argv[0], argv[0], argv[1], argv[2], argv[3]);
        case 5: return funcptr(argv[0], argv[1], argv[2], argv[3], argv[4]);
      }
    }
    case 6: {
      void* (*funcptr)(Box*, Box*, Box*, Box*, Box*, Box*) = f.funcPtr;
      switch (argc) {
        case 0: return funcptr(closure->argv[0], closure->argv[1], closure->argv[2], closure->argv[3], closure->argv[4], closure->argv[5]);
        case 1: return funcptr(closure->argv[0], closure->argv[1], closure->argv[2], closure->argv[3], closure->argv[4], argv[0]);
        case 2: return funcptr(closure->argv[0], closure->argv[1], closure->argv[2], closure->argv[3], argv[0], argv[1]);
        case 3: return funcptr(closure->argv[0], closure->argv[1], closure->argv[2], argv[0], argv[1], argv[2]);
        case 4: return funcptr(closure->argv[0], closure->argv[1], argv[0], argv[1], argv[2], argv[3]);
        case 5: return funcptr(closure->argv[1], argv[0], argv[1], argv[2], argv[3], argv[4]);
        case 6: return funcptr(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
      }
    }
    default:
      printf("AAAA! Unsupported arity %d", f.arity);
      exit(1);
      break;
  };
  //  return NULL;

}

Box* __attribute__ ((pure)) runtimeSelect(Box* tree, Box* ident) {
  Functions* fs = RUNTIME->functions;
  Structs* structs = RUNTIME->structs;
  if (tree->type >= 1000) {
//    printf("Found struct %d\n", tree->type);
//    printf("Found structs %d\n", structs->size);

    // if rhs is not a local ident, nor a function, try to find this field in lhs data structure
    if (ident->type == -1) {
      String* name = ident->value.ptr; // should be identifier name
//      printf("Ident name %.*s\n", name->length, name->bytes);
      Struct* strct = structs->structs[tree->type - 1000]; // find struct in global array of structs
      int numFields = strct->numFields;
      for (int i = 0; i < numFields; i++) {
        String* field = strct->fields[i];
//        printf("Check field %d %.*s\n", field->length, field->length, field->bytes);
        if (field->length == name->length && strncmp(field->bytes, name->bytes, name->length) == 0) {
          Box** values = tree->value.ptr;
          Box* value = values[i];
//          printf("Found value %d at index %d\n", value->type, i);
//          println(toString(value));
          return value;
        }
      }
      printf("Couldn't find field %.*s\n", name->length, name->bytes);
    } else if (ident->type == CLOSURE) {
      // FIXME fix for closure?  check arity?
      Closure* f = unbox(CLOSURE, ident);
      assert(fs->functions[f->funcIdx].arity == 1);
      return runtimeApply(ident, 1, &tree);
    }
  } else if (ident->type == CLOSURE) {
    // FIXME fix for closure?  check arity?
    Closure* f = unbox(CLOSURE, ident);
    assert(fs->functions[f->funcIdx].arity == 1);
    return runtimeApply(ident, 1, &tree);
  }
  return boxError(&s);
}


/* ================== IO ================== */

void putInt(int c) {
  putchar(c + 48);
}

double putchard(double X) {
  putchar((char)X);
  fflush(stdout);
  return 0;
}

void * runtimePutchar(Box* ch) {
  char c = (char) unboxInt(ch);
  putchar(c);
  fflush(stdout);
  return 0;
}

void * println(Box* val) {
  String * str = unbox(STRING, val);
  printf("%.*s\n", str->length, str->bytes);
  return NULL;
}


/* =================== Arrays ================= */


Array* createArray(size_t size) {
  Array * array = gcMalloc(sizeof(Array));
  array->length = size;
  array->data = (size > 0 ) ? gcMalloc(sizeof(Box*) * size) : NULL;
  return array;
}

Box* boxArray(size_t size, ...) {
  va_list argp;
  Array * array = createArray(size);
  va_start (argp, size);
  for (int i = 0; i < size; i++) {
    Box* arg = va_arg(argp, Box*);
    array->data[i] = arg;
  }
  va_end (argp);                  /* Clean up. */
  return box(ARRAY, array);
}

Box* append(Box* arrayValue, Box* value) {
  Array* array = unbox(ARRAY, arrayValue);
  Array * newArray = createArray(array->length + 1);
  memcpy(newArray->data, array->data, sizeof(Box*) * array->length);
  newArray->data[array->length] = value;
  return box(ARRAY, newArray);
}

Box* prepend(Box* arrayValue, Box* value) {
  Array* array = unbox(ARRAY, arrayValue);
  Array * newArray = createArray(array->length + 1);
  memcpy(newArray->data + 1, array->data, sizeof(Box*) * array->length);
  newArray->data[0] = value;
  return box(ARRAY, newArray);
}

Box* __attribute__ ((pure)) makeString(char * str) {
  String* val = gcMalloc(sizeof(String));
  val->length = strlen(str);
  strncpy(val->bytes, str, val->length);
  return box(STRING, val);
}

Box* __attribute__ ((pure)) arrayToString(Box* arrayValue)  {
  Array* array = unbox(ARRAY, arrayValue);
  if (array->length == 0) {
    return makeString("[]");
  } else {
    int seps = array->length * 2;
    size_t resultSize = array->length * 32; // assume 32 bytes per array element. No reason, just guess
    char * result = gcMalloc(resultSize);
    strcat(result, "[");
    size_t curSize = 1;
    for (int i = 0; i < array->length; i++) {
      Box* elem = array->data[i];
      String* value = unbox(STRING, toString(elem));
      if (curSize + value->length >= resultSize + 10) { // reserve 10 bytes for trailing ']' etc
        size_t newSize = resultSize * 1.5;
        result = gcRealloc(result, newSize);
        resultSize = newSize;
      }
      strncat(result, value->bytes, value->length);
      curSize += value->length;
      if (i + 1 < array->length) {
        strcat(result, ", ");
        curSize += 2;
      }
    }
    strcat(result, "]");
    return makeString(result);
  }
}

/* =============== Strings ============= */

Box* __attribute__ ((pure)) toString(Box* value) {
  char buf[100];

  switch (value->type) {
    case UNIT: return UNIT_STRING;
    case BOOL: return makeString(value->value.num == 0 ? "false" : "true");
    case INT:
      snprintf(buf, 100, "%d", value->value.num);
      return makeString(buf);
    case DOUBLE:
      snprintf(buf, 100, "%12.9lf", value->value.dbl);
      return makeString(buf);
    case STRING:  return value;
    case CLOSURE: return makeString("<func>");
    case ARRAY:   return arrayToString(value);
    case -1: {
      String *name = (String *) value->value.ptr;
      printf("AAAA!!! Undefined identifier %.*s\n", name->length, name->bytes);
      exit(1);
    }
    default:
      printf("Unsupported type %d", value->type);
      exit(1);
  }
}

/* =========== Math ================== */

Box* __attribute__ ((pure)) lasqrt(Box * dbl) {
  switch (dbl->type) {
    case INT: return boxInt((int) sqrt(dbl->value.num)); break;
    case DOUBLE: return boxFloat64(sqrt(dbl->value.dbl)); break;
    default: printf("AAAA!!! Type mismatch! Expected Int or Double for lasqrt but got %i\n", dbl->type); exit(1);
  }
}

/* ============ System ================ */

void initEnvironment(int argc, char* argv[]) {
//  int len = 0;
//  for (int i = 0; i< argc; i++) len += strlen(argv[i]);
//  char buf[len + argc*2 + 10];
//  for (int i = 0; i < argc; i++) {
//    strcat(buf, argv[i]);
//    strcat(buf, " ");
//  }
//  printf("Called with %d \n", argc);
  ENV.argc = argc;
  Array* array = createArray(argc);
  for (int i = 0; i < argc; i++) {
    Box* s = makeString(argv[i]);
    array->data[i] = s;
  }
  ENV.argv = box(ARRAY, array);
}

Box* getArgs() {
  return ENV.argv;
}

Box* toInt(Box* s) {
  String* str = unbox(STRING, s);
//  println(s);
  char* cstr = malloc(str->length + 1);
  memcpy(cstr, str->bytes, str->length);
  cstr[str->length] = 0;
//  printf("cstr = %s\n", cstr);
  char *ep;
  long i = strtol(cstr, &ep, 10);
  if (cstr == ep) {
    printf("Couldn't convert %s to int", cstr);
    exit( EXIT_FAILURE );
  }
  free(cstr);
  return boxInt(i);
}

void initLascaRuntime(Runtime* runtime) {
    GC_init();
    RUNTIME = runtime;
    UNIT_STRING = makeString("()");
    for (int i = 0; i < 100; i++) {
      INT_ARRAY[i].type = INT;
      INT_ARRAY[i].value.num = i;
    }
//    printf("Init Lasca 0.0.0.1 runtime. Enjoy :)\n# funcs = %d, # structs = %d\n", RUNTIME->functions->size, RUNTIME->structs->size);
}

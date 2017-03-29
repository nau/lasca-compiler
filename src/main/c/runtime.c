#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <gc.h>

#define REC(n, s) REC(n-1, s##s)
#define FPTR(n) REC(4, 1)

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

struct type_info {
  int type;
  void* value;
};

struct string {
  int length;
  char bytes[];
};

struct closure {
  int funcIdx;
  int argc;
  struct type_info** argv;
};

typedef struct {
  int length;
  struct type_info** data;
} Array;

struct Function {
  struct string* name;
  void * funcPtr;
  int arity;
};

struct Functions {
  int size;
  struct Function functions[];
};

void initLascaRuntime() {
    GC_init();
    printf("Init Lasca 0.0.0.1 runtime. Enjoy :)\n");
}

void *gcMalloc(size_t s) {
    return GC_malloc(s);
}

struct type_info *box(int type_id, void *value) {
  struct type_info* ti = (struct type_info*) gcMalloc(sizeof(struct type_info));
  ti->type = type_id;
  ti->value = value;
  return ti;
}

struct type_info * boxBool(int i) {
//  printf("boxBool(%d) ", i);
  return box(0, (void *) (long) i);
}

struct type_info * boxInt(int i) {
//  printf("boxInt(%d) ", i);
  return box(1, (void *) (long) i);
}

struct type_info * boxFloat64(double i) {
  return box(2, (void *) (long) i);
}

struct type_info * boxClosure(int idx, int argc, struct type_info** args) {
  struct closure* cl = (struct closure*) gcMalloc(sizeof(struct closure));
//  printf("boxClosure(%d, %d, %p)\n", idx, argc, args);
//  fflush(stdout);
  cl->funcIdx = idx;
  cl->argc = argc;
  cl->argv = args;
//  printf("Enclose %d, argc = %d, args[0].type = %d, args[1].type = %d\n", idx, argc, args[0]->type, args[1]->type);
//  fflush(stdout);
  return box(4, (void *) cl);
}

struct type_info * boxFunc(int idx) {
//  printf("boxFunc(%d)\n", idx);
//  fflush(stdout);
  return boxClosure(idx, 0, NULL);
}

void *unbox(struct type_info* ti, int expected) {
//  printf("unbox(%d, %d) ", ti->type, (int) ti->value);
  if (ti->type == expected) {
  	return ti->value;
  } else {
    printf("AAAA!!! Expected %i but got %i\n", expected, ti->type);
    exit(1);
  }
}

struct type_info* runtimeBinOp(int code, struct type_info* lhs, struct type_info* rhs) {
  if (lhs->type != rhs->type) {
  	printf("AAAA!!! Type mismatch! lhs = %i, rhs = %i\n", lhs->type, rhs->type);
  	exit(1);
  }
  int left = (int) lhs->value;
  int right = (int) rhs->value;
  struct type_info* result = NULL;

  switch (code) {
  case ADD: result = boxInt(left + right); break;
  case SUB: result = boxInt(left - right); break;
  case MUL: result = boxInt(left * right); break;
  case DIV: result = boxInt(left / right); break;
  case EQ:  result = boxBool(left == right); break;
  case NE:  result = boxBool(left != right); break;
  case LT:  result = boxBool(left < right); break;
  case LE:  result = boxBool(left <= right); break;
  case GE:  result = boxBool(left >= right); break;
  case GT:  result = boxBool(left > right); break;
  default:
  	printf("AAAA!!! Unsupported binary operation %i", code);
    exit(1);
  }
  return result;
}

struct type_info* __cdecl runtimeApply(struct Functions* fs, struct type_info* val, int argc, struct type_info* argv[]) {
  if (val->type != 4) {
    exit(1);
  }
  struct closure *closure = (struct closure *) val->value;
  if (closure->funcIdx >= fs->size) {
    printf("AAAA!!! No such function with id %d, max id is %d", (int) closure->funcIdx, fs->size);
    exit(1);
  }
  struct Function f = fs->functions[closure->funcIdx];
  if (f.arity != argc + closure->argc) {
    printf("AAAA!!! Function %.*s takes %d params, but passed %d enclosed params and %d params instead",
      f.name->length, f.name->bytes, closure->argc, argc);
    exit(1);
  }
  // TODO: use platform ABI, like it should be.
  // Currently, it's hardcoded for applying a function with up to 6 arguments in dynamic mode
  switch (f.arity) {
    case 0: {
      void* (*funcptr)() = (void* (*)()) f.funcPtr;
      return funcptr();
    }
    case 1: {
      void* (*funcptr)(struct type_info*) = (void* (*)(struct type_info*)) f.funcPtr;
      return (argc == 1) ? funcptr(argv[0]) : funcptr(closure->argv[0]);
    }
    case 2: {
      void* (*funcptr)(struct type_info*, struct type_info*) = (void* (*)(struct type_info*, struct type_info*)) f.funcPtr;
      switch (argc) {
        case 0: return funcptr(closure->argv[0], closure->argv[1]);
        case 1: return funcptr(closure->argv[0], argv[0]);
        case 2: return funcptr(argv[0], argv[1]);
      }
    }
    case 3: {
      void* (*funcptr)(struct type_info*, struct type_info*, struct type_info*) =
        (void* (*)(struct type_info*, struct type_info*, struct type_info*)) f.funcPtr;
        switch (argc) {
          case 0: return funcptr(closure->argv[0], closure->argv[1], closure->argv[2]);
          case 1: return funcptr(closure->argv[0], closure->argv[1], argv[0]);
          case 2: return funcptr(closure->argv[0], argv[0], argv[1]);
          case 3: return funcptr(argv[0], argv[1], argv[2]);
        }
    }
    case 4: {
      void* (*funcptr)(struct type_info*, struct type_info*, struct type_info*, struct type_info*) =
      (void* (*)(struct type_info*, struct type_info*, struct type_info*, struct type_info*)) f.funcPtr;
      switch (argc) {
        case 0: return funcptr(closure->argv[0], closure->argv[1], closure->argv[2], closure->argv[3]);
        case 1: return funcptr(closure->argv[0], closure->argv[1], closure->argv[2], argv[0]);
        case 2: return funcptr(closure->argv[0], closure->argv[1], argv[0], argv[1]);
        case 3: return funcptr(closure->argv[0], argv[0], argv[1], argv[2]);
        case 4: return funcptr(argv[0], argv[1], argv[2], argv[3]);
      }
    }
    case 5: {
      void* (*funcptr)(struct type_info*, struct type_info*, struct type_info*, struct type_info*, struct type_info*) =
      (void* (*)(struct type_info*, struct type_info*, struct type_info*, struct type_info*, struct type_info*)) f.funcPtr;
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
      void* (*funcptr)(struct type_info*, struct type_info*, struct type_info*, struct type_info*, struct type_info*, struct type_info*) =
      (void* (*)(struct type_info*, struct type_info*, struct type_info*, struct type_info*, struct type_info*, struct type_info*)) f.funcPtr;
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


double putchard(double X) {
  putchar((char)X);
  fflush(stdout);
  return 0;
}

void * runtimePutchar(struct type_info* ch) {
  char c = (char) unbox(ch, 1);
  putchar(c);
  fflush(stdout);
  return 0;
}

void * __cdecl println(struct type_info* val) {
  assert(val->type == 3); // it's a string
  struct string * str = (struct string *) val->value;
  printf("%.*s\n", str->length, str->bytes);
  return NULL;
}


//=================== Arrays =================


Array* createArray(size_t size) {
  Array * array = gcMalloc(sizeof(Array));
  array->length = size;
  array->data = (size > 0 ) ? (struct type_info**) gcMalloc(sizeof(struct type_info*) * size) : NULL;
  return array;
}

struct type_info* newArray() {
  return box(5, createArray(0));
}

struct type_info* append(struct type_info* arrayValue, struct type_info* value) {
  assert(arrayValue->type == 5); // it's an array
  Array* array = (Array *) arrayValue->value;
  Array * newArray = createArray(array->length + 1);
  memcpy(newArray->data, array->data, sizeof(struct type_info*) * array->length);
  newArray->data[array->length] = value;
  return box(5, newArray);
}

struct type_info* prepend(struct type_info* arrayValue, struct type_info* value) {
  assert(arrayValue->type == 5); // it's an array
  Array* array = (Array *) arrayValue->value;
  Array * newArray = createArray(array->length + 1);
  memcpy(newArray->data + 1, array->data, sizeof(struct type_info*) * array->length);
  newArray->data[0] = value;
  return box(5, newArray);
}

struct type_info* makeString(char * str) {
  struct string* val = gcMalloc(sizeof(struct string));
  val->length = strlen(str);
  strncpy(val->bytes, str, val->length);
  return box(3, val);
}

void reverse(char s[]) {
     int i, j;
     char c;

     for (i = 0, j = strlen(s)-1; i<j; i++, j--) {
         c = s[i];
         s[i] = s[j];
         s[j] = c;
     }
}

void itoa(int n, char s[]) {
  int i, sign;

  if ((sign = n) < 0)  /* record sign */
     n = -n;          /* make n positive */
  i = 0;
  do {       /* generate digits in reverse order */
     s[i++] = n % 10 + '0';   /* get next digit */
  } while ((n /= 10) > 0);     /* delete it */
  if (sign < 0)
     s[i++] = '-';
  s[i] = '\0';
  reverse(s);
}

struct type_info* arrayToString(struct type_info* arrayValue) {
  assert(arrayValue->type == 5); // it's an array
  Array* array = (Array *) arrayValue->value;
  if (array->length == 0) {
    return makeString("[]");
  } else {
    int seps = array->length * 2;
    char buf[12]; // longest number is −2147483648, it's 11 bytes + 0 termination byte;
    char * result = (char *) gcMalloc(array->length * 12 + seps + 1);
    strcat(result, "[");
    for (int i = 0; i < array->length; i++) {
      struct type_info*elem = (struct type_info*) array->data[i];
      assert(elem->type == 1); // only Int for now
      int value = (int) elem->value;
      itoa(value, buf);
      strcat(result, buf);
      if (i + 1 < array->length)
        strcat(result, ", ");
    }
    strcat(result, "]");
    return makeString(result);
  }
}

struct type_info* arrayApply(struct type_info* arrayValue, struct type_info* idx) {
  assert(arrayValue->type == 5); // it's an array
  assert(idx->type == 1);
  Array* array = (Array *) arrayValue->value;
  int index = (int) idx->value;
  assert(array->length > index);
  return array->data[index];
}

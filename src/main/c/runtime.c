#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <gc.h>


struct type_info {
  int type;
  void* value;
};

struct string {
  int length;
  char bytes[];
};

struct closure {
  void* funcPtr;
  int argc;
  void* argv[];
};


struct Function {
  struct string* name;
  void * funcPtr;
  int arity;
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

struct type_info * boxFunc(int idx) {
  return box(4, (void *) (long) idx);
}

void *unbox(struct type_info* ti, int expected) {
//  printf("unbox(%d, %d) ", ti->type, (int) ti->value);
  if (ti->type == expected) {
  	return ti->value;
  } else {
    printf("AAAA!!! Expected %i but got %i", expected, ti->type);
    exit(1);
  }
}


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

struct type_info* runtimeBinOp(int code, struct type_info* lhs, struct type_info* rhs) {
  if (lhs->type != rhs->type) {
  	printf("AAAA!!! Type mismatch! lhs = %i, rhs = %i", lhs->type, rhs->type);
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

struct type_info* runtimeApply(struct Function fs[], int size, struct type_info* func, struct type_info* args[], int argc) {
  if (func->type != 4) {
    printf("AAAA!!! Type mismatch! expected function but called on %d", func->type);
    exit(1);
  }
  if ((int)func->value >= size) {
    printf("AAAA!!! No such function with id %d", (int) func->value);
    exit(1);
  }
  int idx = (int)func->value;
  struct Function f = fs[idx];
  switch (f.arity) {
    case 0: {
      void* (*funcptr)() = (void* (*)()) f.funcPtr;
      return funcptr();
    }
  	case 1: {
  	  void* (*funcptr)(struct type_info*) = (void* (*)(struct type_info*)) f.funcPtr;
      return funcptr(args[0]);
  	}
    case 2: {
	  void* (*funcptr)(struct type_info*, struct type_info*) = (void* (*)(struct type_info*, struct type_info*)) f.funcPtr;
	  return funcptr(args[0], args[1]);
    }
    case 3: {
	  void* (*funcptr)(struct type_info*, struct type_info*, struct type_info*) =
	    (void* (*)(struct type_info*, struct type_info*, struct type_info*)) f.funcPtr;
	  return funcptr(args[0], args[1], args[2]);
	}
	case 4: {
	  void* (*funcptr)(struct type_info*, struct type_info*, struct type_info*, struct type_info*) =
		(void* (*)(struct type_info*, struct type_info*, struct type_info*, struct type_info*)) f.funcPtr;
	  return funcptr(args[0], args[1], args[2], args[3]);
	}
	case 5: {
	  void* (*funcptr)(struct type_info*, struct type_info*, struct type_info*, struct type_info*, struct type_info*) =
		(void* (*)(struct type_info*, struct type_info*, struct type_info*, struct type_info*, struct type_info*)) f.funcPtr;
	  return funcptr(args[0], args[1], args[2], args[3], args[4]);
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

void * println(struct type_info* val) {
  assert(val->type == 3); // it's a string
  struct string * str = (struct string *) val->value;
  printf("%.*s\n", str->length, str->bytes);
  return NULL;
}

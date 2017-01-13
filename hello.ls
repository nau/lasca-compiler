extern def sleep(s: Int): Int;
extern def putchar(c: Int): Int;

def lascamain = test 60

def test(i) = {
  if i == 30 then {
    putchar 10
  } else {
    putchar i;
    test (i - 1);
  }
}

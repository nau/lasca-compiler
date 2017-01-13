def lascamain = {
  test(60);
}

def test(i) = {
  if (i == 30) then {
    putchar(99);
  } else {
    putchar(i);
    putchar(10);
    test(i - 1);
  }
}

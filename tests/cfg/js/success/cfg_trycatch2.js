var x, y;
x = 5;
try { 
  x = 3;
  try {
    y = 103;
    if (y > x*10) y /= 2;
    else {}
  } catch (y) {
    ;;;;;;;
  }
}
catch (x) {
  x = 2;
}


  {
    var __result1 = "1" * "1" !== 1;
    var __expect1 = false;
  }
  {
    var __result2 = new String("1") * "1" !== 1;
    var __expect2 = false;
  }
  {
    var __result3 = "1" * new String("1") !== 1;
    var __expect3 = false;
  }
  {
    var __result4 = new String("1") * new String("1") !== 1;
    var __expect4 = false;
  }
  {
    var __result5 = isNaN("x" * "1") !== true;
    var __expect5 = false;
  }
  {
    var __result6 = isNaN("1" * "x") !== true;
    var __expect6 = false;
  }
  
  {
    var __result1 = Boolean.prototype.valueOf() !== false;
    var __expect1 = false;
  }
  {
    var __result2 = (new Boolean()).valueOf() !== false;
    var __expect2 = false;
  }
  {
    var __result3 = (new Boolean(0)).valueOf() !== false;
    var __expect3 = false;
  }
  {
    var __result4 = (new Boolean(- 1)).valueOf() !== true;
    var __expect4 = false;
  }
  {
    var __result5 = (new Boolean(1)).valueOf() !== true;
    var __expect5 = false;
  }
  {
    var __result6 = (new Boolean(new Object())).valueOf() !== true;
    var __expect6 = false;
  }
  
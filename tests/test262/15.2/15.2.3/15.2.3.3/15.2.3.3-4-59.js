  function testcase() 
  {
    var desc = Object.getOwnPropertyDescriptor(Array.prototype, "reduce");
    if (desc.value === Array.prototype.reduce && desc.writable === true && desc.enumerable === false && desc.configurable === true)
    {
      return true;
    }
  }
  {
    var __result1 = testcase();
    var __expect1 = true;
  }
  
  function testcase() 
  {
    var desc = Object.getOwnPropertyDescriptor(Object.prototype, "isPrototypeOf");
    if (desc.value === Object.prototype.isPrototypeOf && desc.writable === true && desc.enumerable === false && desc.configurable === true)
    {
      return true;
    }
  }
  {
    var __result1 = testcase();
    var __expect1 = true;
  }
  
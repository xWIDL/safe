  function testcase() 
  {
    var obj = {
      "" : 1
    };
    var desc = Object.getOwnPropertyDescriptor(obj, "");
    return desc.value === 1;
  }
  {
    var __result1 = testcase();
    var __expect1 = true;
  }
  
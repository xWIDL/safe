  function testcase() 
  {
    var obj = {
      
    };
    Object.defineProperties(obj, {
      property : {
        configurable : new Boolean(true)
      }
    });
    var preCheck = obj.hasOwnProperty("property");
    delete obj.property;
    return preCheck && ! obj.hasOwnProperty("property");
  }
  {
    var __result1 = testcase();
    var __expect1 = true;
  }
  
  function testcase() 
  {
    var obj = {
      
    };
    Object.defineProperties(obj, {
      property : {
        writable : 0
      }
    });
    obj.property = "isWritable";
    return obj.hasOwnProperty("property") && typeof (obj.property) === "undefined";
  }
  {
    var __result1 = testcase();
    var __expect1 = true;
  }
  
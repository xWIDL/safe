  function testcase() 
  {
    var obj = {
      
    };
    Object.defineProperties(obj, {
      property : {
        writable : "abc"
      }
    });
    obj.property = "isWritable";
    return obj.property === "isWritable";
  }
  {
    var __result1 = testcase();
    var __expect1 = true;
  }
  
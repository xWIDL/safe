  function testcase() 
  {
    var obj = {
      
    };
    Object.defineProperty(obj, "property", {
      writable : [1, 2, 3, ]
    });
    var beforeWrite = obj.hasOwnProperty("property");
    obj.property = "isWritable";
    var afterWrite = (obj.property === "isWritable");
    return beforeWrite === true && afterWrite === true;
  }
  {
    var __result1 = testcase();
    var __expect1 = true;
  }
  
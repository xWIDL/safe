  function testcase() 
  {
    var obj = {
      
    };
    var dateObj = new Date();
    dateObj.configurable = true;
    Object.defineProperty(obj, "property", dateObj);
    var beforeDeleted = obj.hasOwnProperty("property");
    delete obj.property;
    var afterDeleted = obj.hasOwnProperty("property");
    return beforeDeleted === true && afterDeleted === false;
  }
  {
    var __result1 = testcase();
    var __expect1 = true;
  }
  
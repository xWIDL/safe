  function testcase() 
  {
    try
{      Object.defineProperty(RegExp.prototype, "prop", {
        value : 1001,
        writable : true,
        enumerable : true,
        configurable : true
      });
      var regObj = new RegExp();
      var verifyEnumerable = false;
      for(var p in regObj)
      {
        if (p === "prop")
        {
          verifyEnumerable = true;
        }
      }
      return ! regObj.hasOwnProperty("prop") && verifyEnumerable;}
    finally
{      delete RegExp.prototype.prop;}

  }
  {
    var __result1 = testcase();
    var __expect1 = true;
  }
  
  function testcase() 
  {
    var o = {
      
    };
    Object.defineProperty(o, "foo", {
      set : (function () 
      {
        ;
      })
    });
    return o.hasOwnProperty("foo");
  }
  {
    var __result1 = testcase();
    var __expect1 = true;
  }
  
  function testcase() 
  {
    var obj = {
      
    };
    Object.preventExtensions(obj);
    try
{      Object.defineProperties(obj, {
        prop : {
          value : 12,
          configurable : true
        }
      });
      return false;}
    catch (e)
{      return e instanceof TypeError && ! obj.hasOwnProperty("prop");}

  }
  {
    var __result1 = testcase();
    var __expect1 = true;
  }
  
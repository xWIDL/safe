  function testcase() 
  {
    var arr = [0, 1, ];
    Object.defineProperty(arr, "1", {
      value : 1,
      configurable : false
    });
    try
{      Object.defineProperties(arr, {
        length : {
          value : 1
        }
      });
      return false;}
    catch (ex)
{      var desc = Object.getOwnPropertyDescriptor(arr, "length");
      return ex instanceof TypeError && desc.value === 2 && desc.writable && ! desc.enumerable && ! desc.configurable;}

  }
  {
    var __result1 = testcase();
    var __expect1 = true;
  }
  
  function testcase() 
  {
    function f(o) 
    {
      function innerf(o, x) 
      {
        try
{          throw o;}
        catch (e)
{          return x;}

      }
      return innerf(o, 42);
    }
    if (f({
      
    }) === 42)
    {
      return true;
    }
  }
  {
    var __result1 = testcase();
    var __expect1 = true;
  }
  
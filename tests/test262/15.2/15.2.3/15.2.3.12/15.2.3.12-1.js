  function testcase() 
  {
    try
{      Object.isFrozen(0);}
    catch (e)
{      if (e instanceof TypeError)
      {
        return true;
      }}

  }
  {
    var __result1 = testcase();
    var __expect1 = true;
  }
  
//   TODO dataPropertyAttributesAreCorrect
//   function testcase() 
//   {
//     var obj = {
//       
//     };
//     Object.defineProperty(obj, "foo", {
//       value : "abcd",
//       writable : false,
//       configurable : false
//     });
//     try
// {      Object.defineProperties(obj, {
//         foo : {
//           value : "defg"
//         }
//       });
//       return false;}
//     catch (e)
// {      return (e instanceof TypeError) && dataPropertyAttributesAreCorrect(obj, "foo", "abcd", false, false, false);}
// 
//   }
//   {
//     var __result1 = testcase();
//     var __expect1 = true;
//   }
//   

//   TODO getter/setter
//   function testcase() 
//   {
//     var obj = {
//       
//     };
//     var getFunc = (function () 
//     {
//       return 1001;
//     });
//     Object.defineProperty(obj, "prop", {
//       get : getFunc,
//       set : undefined,
//       enumerable : true,
//       configurable : true
//     });
//     var desc1 = Object.getOwnPropertyDescriptor(obj, "prop");
//     Object.defineProperty(obj, "prop", {
//       value : 1001
//     });
//     var desc2 = Object.getOwnPropertyDescriptor(obj, "prop");
//     return desc1.hasOwnProperty("get") && desc2.hasOwnProperty("value");
//   }
//   {
//     var __result1 = testcase();
//     var __expect1 = true;
//   }
//   

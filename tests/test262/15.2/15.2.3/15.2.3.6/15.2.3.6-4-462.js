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
//     var verifySetFunc = "data";
//     var setFunc = (function (value) 
//     {
//       verifySetFunc = value;
//     });
//     Object.defineProperty(obj, "prop", {
//       get : undefined,
//       set : setFunc,
//       enumerable : true,
//       configurable : true
//     });
//     var result1 = typeof obj.prop === "undefined";
//     var desc1 = Object.getOwnPropertyDescriptor(obj, "prop");
//     Object.defineProperty(obj, "prop", {
//       get : getFunc
//     });
//     var result2 = obj.prop === 1001;
//     var desc2 = Object.getOwnPropertyDescriptor(obj, "prop");
//     return result1 && result2 && typeof desc1.get === "undefined" && desc2.get === getFunc;
//   }
//   {
//     var __result1 = testcase();
//     var __expect1 = true;
//   }
//   

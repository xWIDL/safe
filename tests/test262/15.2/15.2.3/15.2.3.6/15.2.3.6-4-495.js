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
//     var propertyDefineCorrect = obj.hasOwnProperty("prop");
//     var desc = Object.getOwnPropertyDescriptor(obj, "prop");
//     return propertyDefineCorrect && typeof desc.set === "undefined";
//   }
//   {
//     var __result1 = testcase();
//     var __expect1 = true;
//   }
//   

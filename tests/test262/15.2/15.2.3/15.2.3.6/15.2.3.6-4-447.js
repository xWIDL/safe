//   TODO getter/setter
//   function testcase() 
//   {
//     var obj = {
//       
//     };
//     Object.defineProperty(obj, "prop", {
//       get : undefined,
//       set : undefined,
//       enumerable : false,
//       configurable : true
//     });
//     var desc1 = Object.getOwnPropertyDescriptor(obj, "prop");
//     Object.defineProperty(obj, "prop", {
//       configurable : false
//     });
//     var desc2 = Object.getOwnPropertyDescriptor(obj, "prop");
//     delete obj.prop;
//     return desc1.configurable === true && desc2.configurable === false && obj.hasOwnProperty("prop");
//   }
//   {
//     var __result1 = testcase();
//     var __expect1 = true;
//   }
//   

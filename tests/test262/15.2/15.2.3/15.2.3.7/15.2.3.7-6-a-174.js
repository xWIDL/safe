//   TODO [[DefineOwnProperty]] for Array object
//   function testcase() 
//   {
//     var arr = [0, 1, ];
//     Object.defineProperties(arr, {
//       length : {
//         value : 1
//       }
//     });
//     return ! arr.hasOwnProperty("1");
//   }
//   {
//     var __result1 = testcase();
//     var __expect1 = true;
//   }
//   

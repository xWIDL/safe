//   TODO getter/setter
//   function testcase() 
//   {
//     var arrObj = [];
//     function setFunc(value) 
//     {
//       arrObj.setVerifyHelpProp = value;
//     }
//     Object.defineProperty(arrObj, "0", {
//       set : setFunc,
//       configurable : true
//     });
//     Object.defineProperty(arrObj, "0", {
//       configurable : false
//     });
//     return accessorPropertyAttributesAreCorrect(arrObj, "0", undefined, setFunc, "setVerifyHelpProp", 
//     false, 
//     false);
//   }
//   {
//     var __result1 = testcase();
//     var __expect1 = true;
//   }
//   

var x;
if (__TOP) 
	x = undefined;
else if (__TOP) 
	x = null;
else if (__TOP) 
	x = true;
else if (__TOP) 
	x = 1;
else if (__TOP)
	x = "str";
else
	x = {};
		
x.p = 1;
	
var __result1 = x.p;
var __expect1 = 1;

var __result2 = x.p;
var __expect2 = undefined;

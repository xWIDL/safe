var o1;
var o2;

if (__TOP)
	o1 = {};
else
	o1 = [];

if (__TOP)
	o2 = Object;
else
	o2 = Array;

var __result1 = o1 instanceof o2
var __expect1 = __BoolTop

var __result2 = o1 instanceof o2
var __expect2 = __BoolTop

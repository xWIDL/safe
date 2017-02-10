var foo = new Foo();

foo.makeBiggerFive();

foo.needBiggerFour(); // should be okay

foo.count = 3;

foo.needBiggerFour(); // should be problematic

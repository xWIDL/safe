var foo = new Foo();

foo.makeRel(); // foo.count >= foo.count2

foo.add(1); // foo.count >= foo.count2 + 1

foo.checkRel(); // should be okay

// foo.add(-2); // should be problematic

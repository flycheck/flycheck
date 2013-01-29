/** A syntax error caused by a missing quote */

(function() {
    var foo = ["Hello world"];
    foo.indexOf("Foo);
}());

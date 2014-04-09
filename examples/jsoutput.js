function kitePlus(a, b){
    return a + b;
};
foo = function(x) {return kitePlus(x,2);};a = kitePlus(2,foo(2));console.log(a);

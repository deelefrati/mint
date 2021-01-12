function foo(){
   const a = 1+1;
   assert(a === 2); 
}

const a =  3;
foo()
assert(a === 3);

function fib(n: number) {
    if (n <= 1) {
      return n;
    }
    return fib(n - 2) + fib(n - 1);
}

assert(fib(0) === 0);
assert(fib(1) === 1);
assert(fib(2) === 1);
assert(fib(10) === 55);
assert(fib(20) === 6765);


function is_even(n: number) {
  if (n === 0) {
    return true;
  }else{
    return is_odd(n - 1);
  }
}

function is_odd(n: number){
  if (n === 0) {
        return false; 
  } else {
    return is_even(n - 1);
  }
}

assert(is_even(10) === true);
assert(is_even(9) === false);
assert(is_even(0) === true);
assert(is_even(1) === false);


assert(is_odd(10) === false);
assert(is_odd(9) === true);
assert(is_odd(0) === false);
assert(is_odd(1) === true);

function only_return(){
 return; 
}

function nothing(){}

assert(only_return() === null);
assert(nothing() === null);

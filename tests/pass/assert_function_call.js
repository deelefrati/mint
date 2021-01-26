function foo(){
   const a = 1+1;
   assert(a === 2);
}

const a =  3;
assert(foo() === null);
assert(a === 3);

function fib(n: number) : number {
    if (n <= 1) {
      return n;
    }
    return fib(n - 2) + fib(n - 1);
}

assert(fib(0) === 0);
assert(fib(1) === 1);
assert(fib(2) === 1);
assert(fib(5) === 5);



function only_return(){
 return;
}

function nothing(){}

assert(only_return() === null);
assert(nothing() === null);

function f(x : boolean) : string {
  if(x){
    return "string";
  }else{
    return "another string";
  }
}

assert(f(true) === "string");
assert(f(false) === "another string");



function g(x: number): number {
  if(x){
    return x;
  }else{
    return x;
  }
}

function h(x: number): number {
  if(x){
    if(x){

    }else{

    }
    return x;
  }else{
    return x;
  }
}

function i(x: number): number {
  if(x){
  }
  return x;
}


function j(x: number): number {
  if(x){
    if(x){
     return x;
    }else{
     return x;
    }
  }else{
    return x;
  }
}

function k(x: number) : number {
  if (x){
    return x;
  }else{
   if(x){
    if(x){

    }else{

    }
     return x;
   }else{
    return x;
   }
  }
}

function l(x: number) : number {
  if(x){
    return x;
  }else{
   return x;
  }

  if(x){

  }else{

  }
}
// some random comment
function foo() : number{
    const a = 1;
   return a;
}


foo();
const a = 10;

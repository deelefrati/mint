function foo() {
  const a = 1 + 1;
  console.assert(a === 2);
}

const a = 3;
console.assert(foo() === null);
console.assert(a === 3);

function fib(n: number): number {
  if (n <= 1) {
    return n;
  }
  return fib(n - 2) + fib(n - 1);
}

console.assert(fib(0) === 0);
console.assert(fib(1) === 1);
console.assert(fib(2) === 1);
console.assert(fib(5) === 5);

function only_return() {
  return;
}

function nothing() {}

console.assert(only_return() === null);
console.assert(nothing() === null);

function f(x: boolean): string {
  if (x) {
    return "string";
  } else {
    return "another string";
  }
}

console.assert(f(true) === "string");
console.assert(f(false) === "another string");

function g(x: number): number {
  if (x) {
    return x;
  } else {
    return x;
  }
}

function h(x: number): number {
  if (x) {
    if (x) {
    } else {
    }
    return x;
  } else {
    return x;
  }
}

function i(x: number): number {
  if (x) {
  }
  return x;
}

function j(x: number): number {
  if (x) {
    if (x) {
      return x;
    } else {
      return x;
    }
  } else {
    return x;
  }
}

function k(x: number): number {
  if (x) {
    return x;
  } else {
    if (x) {
      if (x) {
      } else {
      }
      return x;
    } else {
      return x;
    }
  }
}

function l(x: number): number {
  if (x) {
    return x;
  } else {
    return x;
  }

  if (x) {
  } else {
  }
}
// some random comment
function fo0o(): number {
  const b = 1;
  return b;
}

fo0o();
const b = 10;

function is_odd(x: number) : boolean {
    if(x === 0){
        return false;
    }
    return is_even(x-1);
}

function is_even(x: number) : boolean {
    if(x === 0) {
        return true;
    }
    return is_odd(x-1);
}

console.assert(is_odd(1));
console.assert(is_odd(3));
console.assert(is_odd(5));
console.assert(!is_odd(0));

console.assert(is_even(0));
console.assert(is_even(2));
console.assert(is_even(4));
console.assert(!is_even(1));
const a1 = "a";
function f_1() : string{
    const a1 = "another a";
    function g() : string {
        return a1;
    }
    return g();
}

console.assert(f_1() === "another a");

const x1 = "global";
if(true) {
    const x1 = "block";
    function show_x(): string {
        return x1;
    }

    console.assert(show_x() === "block");
}



# MINT

Mint is a minimal subset of TypeScript designed for teaching programming

### Features implemented

- [x] Primitive types
- [x] Arithmetic operations between primitive types
- [x] Logical operations
- [x] Variables
- [x] Scope
- [x] Print statement
- [x] If statement
- [x] Functions
- [x] User Type
- [x] Unions


## Instalation

### Dependencies

- Rust Programming Language compiler.
  - To install it, follow the official guide [here](https://www.rust-lang.org/tools/install)

> Tip: you can check if you already have Rust installed by typing `cargo --version` in a terminal.
> (If a message like "cargo x.xx.x" returns by the command, it is already installed)

### Instalation proccess

1. Clone or [download](https://github.com/deelefrati/mint.git) this repository.
2. In a terminal, enter the repository's folder:

```bash
cd mint
```

3. Finally, to install the interpreter, use

```bash
cargo install --path .
```

## Language guide

### Primitive types

- `Number` for numeric values:

  ```typescript
  1, 0.5, 0, -0.5, -1
  ```

- `String` for string literals (words or phrases surrounded by `"`):

  ```typescript
  "Hello, world!"
  ```

- `bool` for boolean types:

  ```typescript
  true, false
  ```
There is also the `null` type, which represents an empty value.


### Operators
Mint has the following types of operators:

#### Assignment operator
| Operator  | Usage | Description |
| :-----: | :-----: | :----------: |
| = | x = y |  Assigns the value of its right operand to its left operand|

#### Comparison operators
| Operator  | Usage | Description |
|:-----: | :-----: | :----------: |
|=== | 10 === 10 | Returns true if the operands are equal and of the same type |
|!== | 10 !== 11 | Returns true if the operands are of the same type but not equal, or are of different type. |
|>   | 1 > 0     | Returns true if the left operand is greater than the right operand.|
|>=  | 1 >= 0    | Returns true if the left operand is greater than or equal to the right operand. 	 |
|<   | 0 < 1     | Returns true if the left operand is less than the right operand.  |
|<=  | 0 <= 1    | Returns true if the left operand is less than or equal to the right operand. |

#### Arithmetic operators
| Operator  | Usage | Description |
|:-----: | :-----: | :----------: |
|+ | 10 + 10 | Returns the addition between left and right operands.|
|- | 10 - 10 | Returns the subtraction between left and right operands.|
|* | 10 * 10 | Returns the multiplication between left and right operands.|
|/ | 10 / 10 | Returns the division between left and right operands.|
|% | 10 % 10 | Returns the integer remainder of dividing the two operands.  |

#### Logical operators
| Operator  | Usage | Description |
|:-----: | :-----: | :----------: |
|&& | true && true    | Returns true if both operands are true; otherwise, returns false. |
|\|\| | true \|\| false | Returns true if either operand is true; if both are false, returns false. |
|!  | !true           | Returns false if its single operand that can be converted to true; otherwise, returns true. |

### Variables

Since Mint is stateless, all variables are constants and cannot be reassigned. 

```typescript
const some_number = 10;

const some_string = "Hello, word!";

const some_bool = true; 

const come_null = null;
```

You can ensure the type of a constant by declaring a type after its name:

```typescript
const some_number : number = 10;
const some_string : string = "hello, world!";

const some_var: str = 10 # this will raise an error
```

### Union
A union type is type formed from two or more other types, representing values that may be any one of those types. It's possible to write a union like this:

```typescript
const id : number | string = 007;
```

Unions can be used in define parameters type and returns types as well.

```typescript
function printId(id: number | string) {
  console.log("Your ID is: " + id);
}

function returnStringOrNumber(n : number) : string | number {
    if (number > 0) {
        return n;
    }else{
        return "Invalid";
    }
}
```

### User defined types

To define a new type, use the keyword `type` folowed by the name of the new type.

Next, define the attributes and their types.

```typescript
type Point = {
    x: number,
    y: number,
};
```

You can create a new instance of the type like this: 


```typescript
    const point : Point = {
        x: 10,
        y: 10,
    };
```

Unlike primitive types, to declare a user type constant it is mandatory to specify the type after the variable name;

### Functions

Function definitions start with the keyword `function` followed by:

- the functions name;
- its parameters around parentheses;
- the type of value it will produce (if any) after a `:` and;
- the function's body inside a pair of brackets , like in the examples:

```typescript
function add_numbers(a: number, b : number) : number {
    return a + b;
}
```

You can call any function like:

```typescript
add_numbers(1, 2);
```

> You can write any valid code inside a function body.

> The keyword `return` will end the execution of the function and return the value produced by the expression in front of it to the function's caller

### Control flow

Control flow can be done with `if/else`

> The if condition has to evaluate to a boolean value or a number (0 - false / any other number = true);

```typescript
    const num = 3;

    if (num < 5) {
        console.log("condition was true");
    } else {
        console.log("condition was false");
    }
```

> You can write any valid code inside a if/else body.

#### Type reninement
When working with joins, it is common to need to specify the type. For this, we use the `typeof` operator to distinguish the types. 

```typescript
function (x: string | number | boolean) {
    if (typeof x === "string"){
        console.log(x); // In this scope, the type of 'x' is only string.
    }else {
        console.log(x); // Right here, 'x' type is 'number | boolean';
        if (typeof x === "number"){
            console.log(x); // Once we refined 'x' again, his type is number;
        }else{
            console.log(x); // Finally, 'x' type is boolean;
        }
    }
}
```

For unions containing only primitive types, the `typeof` operator can do the job for the refinement. However, if we have a union of two or more user types it's not possible to differentiate them. Like the following example:

```typescript
type FirstType = {
    x: number,
}


type SecondType = {
    x: string,
}

function show(param: FirstType | AnotherType) : number {
    return param.x; // There's no way to specify if the field 'x' is number or string before runtime.
} 
```

To resolve this case, it's necessary to create a field with a literal type and verify like this:

```typescript
type FirstType = {
    label: "first type", 
    x: number,
}


type SecondType = {
    label: "second type", 
    x: string,
}

function show(param: FirstType | AnotherType) : number {
    if (param.label === "first type") {
        return param.x; // param is now from type 'FirstType', thus the type of the field 'x' is only number. 
    } else {
        param.x; // param is now from type 'SecondType', thus the type of the field 'x' is only string.
        return 0;
    }
}
```

### Assertion

It is useful to be sure about the correctness of values. The function `console.assert()` will evaluate any expression that returns a boolean value, if it's true then the code will continue to be executed, but if evaluates to false, an error will be eraised.

```typescript
console.assert (10 === 10);
```

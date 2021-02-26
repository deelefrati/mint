type FirstType = {
 x: number,
 y: number,
};

type SecondType = {
 x: string,
};

type Temperatura = FirstType | number;

function f() : Temperatura {
    const a : Temperatura = {
        x: 10,
        y: 20,
    };
    return a;
}

function g() : Temperatura {
    const a : FirstType = {
        x: 10,
        y: 20,
    };
    return a;
}



const a = 10;
const b : number | string = "test";
const c : SecondType = {x: "test",};

console.assert(typeof "10" === "string");
console.assert(typeof true === "boolean");
console.assert(typeof a === "number");
console.assert(typeof b === "string");
console.assert(typeof f() === "object");
console.assert(typeof c === "object");
console.assert(typeof Temperatura === "object");

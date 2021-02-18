
type FirstType = {
 x: number,
 y: number,
};
type SecondType = {
 x: string,
};

const p : FirstType | SecondType | string  = {x:10, y:20,};
const p1 : FirstType | SecondType | string  = {x:"string",};
const p2 : FirstType | SecondType | string  = "teste";
console.assert(p.x === 10 && p.y === 20);
console.assert(p1.x === "string");
console.assert(p2 === "teste");

function foo() : FirstType | number {
    const a : FirstType= {
        x: 2,
        y:2,
    };
    return a;
}
const a: number | string | true = 10;
const b: number | string | true = "10";
const c: number | string | true = true;
const d: number | string | boolean = false;
const e: 10 | 20 | 30 = 10;
const f: 10 | 20 | 30 = 20;
const g: 10 | number = 20;
const h: 10 | number = 10;

console.assert(a === 10);
console.assert(b === "10");
console.assert(c);
console.assert(!d);
console.assert(e === 10);
console.assert(f === 20);
console.assert(g === 20);
console.assert(h === 10);


function union(): number | boolean {
    return true;
}

console.assert(union() === true);


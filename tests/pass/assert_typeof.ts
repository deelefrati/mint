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


const a = 10;
const b : number | string = "test";
const c : SecondType = {x: "test",};

console.assert(typeof "10" === "string");
console.assert(typeof true === "boolean");
console.assert(typeof a === "number");
console.assert(typeof b === "string");
console.assert(typeof f() === "object");
console.assert(typeof c === "object");

type Point = {
    x: "point",
    y: number,
};

type AnotherPoint = {
    x: "another point",
    y: number,
};


type Type_ =  string | Point | number | boolean;


const x : Type_ = {x: "point", y: 2,};


function i(x: Type_) : number  {
    if(typeof x === "object"){
       return x.y;
    }else{
        return 0;

    }
}

function g(x: Type_) : number {
    if(typeof x === "number"){
        return x;
    }else{
        return 0;
   }
}

function h(x: Type_) : string  {

   if(typeof x === "string"){
        return x;
   }else{
       return "test";

   }

}

console.assert(i(x) === 2); 
console.assert(g(x) === 0);
console.assert(h(x) === "test");

type RandomType = {
   x: number, 
};

type first_alias = number;
type second_alias = string;
type third_alias = boolean;
type fourth_alias = 10;
type fifth_alias = "string";
type sixty_alias = true;

const a : first_alias = 10;
const b : second_alias = "hello";
const c : third_alias = true;
const d : fourth_alias = 10;
const e : fifth_alias = "string";
const f : sixty_alias = true;

console.assert(a === 10);
console.assert(b === "hello");
console.assert(c === true);
console.assert(d === 10);
console.assert(e === "string");
console.assert(f === true);

type seventh_alias = number | string | boolean;

const g : seventh_alias = 10;
const h : seventh_alias = "hello";
const i : seventh_alias = false;

type eighth_alias = RandomType;

const j : eighth_alias = {x: 10,};


type ninth_alias = RandomType | number;

const k : ninth_alias = {x: 10,};
const l : ninth_alias = 10;

function f1_alias() : first_alias {
    return 10;
}


function f2_alias() : seventh_alias {
    return 10;
}

function f3_alias() : ninth_alias {
    const k : ninth_alias = {x: 10,};
    return k;
}

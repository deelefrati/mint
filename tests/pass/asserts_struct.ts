type Point = {
  x: number,
  y: number,
};

const point: Point = {
  x: 1,
  y: 2,
};

console.assert(point.x === 1);
console.assert(point.y === 2);

const p2: Point = {
  y: 10,
  x: 20,
};

console.assert(p2.x === 20);
console.assert(p2.y === 10);

type Type = {
  a: string,
};
type AnotherType = {
  x: Type,
  y: number,
};

const tipo: Type = {
  a: "aa",
};

function f(a: Type, b: number): AnotherType {
  const p: AnotherType = {
    x: a,
    y: b,
  };
  return p;
}

const p3 = f(tipo, 10);

console.assert(p3.y === 10);
console.assert(p3.x.a === "aa");

type Type_ = {
    x: number,
    y: number,
};

type AnotherType_ = {
    x: number,
};

type AnotherAnotherType = {
    x: number,
    y: number,
};

const t1 : Type_ = {
    x: 10,
    y: 10,
};

const t2 : Type_ = {
    x: 10,
    y: 10,
};

const t3 : Type_ = {
    x: 0,
    y: 10,
};

const at1 : AnotherType_ = {
    x: 10,
};

const aat1 : AnotherAnotherType = {
    x: 10,
    y: 10,
};

//console.assert(t1 === t2);
//console.assert(!(t1 === t3));
//console.assert(t1 !== t3);
//console.assert(!(t1 !== t2));
//console.assert(!(t1 === at1));
//console.assert(t1 !== aat1);


type Point = {
  x: number;
  y: number;
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

console.assert(p2.x === 10);
console.assert(p2.y === 20);

type Type = {
  a: string;
};
type AnotherType = {
  x: Type;
  y: number;
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


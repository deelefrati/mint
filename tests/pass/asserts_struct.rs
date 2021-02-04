type Point = {
  x: number;
  y: number;
};

const point: Point = {
  x: 1,
  y: 2,
};

assert(point.x === 1);
assert(point.y === 2);

const p2: Point = {
  y: 10,
  x: 20,
};

assert(p2.x === 10);
assert(p2.y === 20);

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

assert(p3.y === 10);
assert(p3.x.a === "aa");


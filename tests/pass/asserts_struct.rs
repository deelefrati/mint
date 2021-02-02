type Point = {
  x: number;
  y: number;
};

const point: Point = {
  x: 1,
  y: 2,
};

assert(point.x);
assert(point.y);

const p2: Point = {
  y: 10,
  x: 20,
};

assert(p2.x);
assert(p2.y);

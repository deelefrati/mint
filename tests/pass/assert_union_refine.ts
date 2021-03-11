type Point = {
    x: "point",
    y: number,
};

type AnotherPoint = {
    x: "another point",
    y: number,
    z: string,
};

type AnotherAnotherPoint = {
    x: "another another point",
    y: number,
};


type Type_ =   Point | AnotherPoint;
type Type2 =   Point | AnotherPoint | AnotherAnotherPoint;

const p : Point = {
    x: "point",
    y: 10,
};

const type_ : Type_ = {
    x: "point",
    y: 2,
};

function f(ty: Type_) : Point {
    if (ty.x === "point"){
        return ty;
    }else{
        const p : Point = {
            x: "point",
            y: 10,
        };
        return p;
    }
}

const awnser = f(type_);
console.assert(awnser.x === "point");
console.assert(awnser.x === "point");

function g(ty: Type2) : AnotherPoint | AnotherAnotherPoint {
    if (ty.x === "point"){
        const p : AnotherPoint = {
            x: "another point",
            y: 10,
            z: "string",
        };
        return p;
    }else{
        return ty;
    }
}
type t = Point | string;

function h(x: t) : string {
    if (typeof x === "object") {
        return x.x;
    }
    return x;
}

function i(x: t) : string {
    if (typeof x === "string") {
        return x;
    }
    return x.x;
}




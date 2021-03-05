type Ty = {
    x: "point",
    y: number,
    z: number,
};

type AnotherTy = {
    x: "another point",
    y: number,
};


type alias_ty=   AnotherTy | Ty;


const x_type : alias_ty = {
    x: "another point",
    y: 2,
};

const y_type : Ty = {
    x : "point",
    y: 10,
    z: 10,
};

function f_get(x: alias_ty) : number  {
    return x.y;
}
function g_get(x: Ty) : "point" {
    return x.x;
}
function h_get(x: AnotherTy | Ty) : number  {
    return x.y;
}

console.assert(f_get(x_type)=== 2); 
console.assert(g_get(y_type)=== "point");
console.assert(h_get(x_type)=== 2);


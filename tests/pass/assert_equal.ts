import {equal} from "MintEqual";


type Point = {
    x: number,
    y: number,
};

const p1 : Point = {
    x: 10,
    y: 20,
}; 
const p2 : Point = {
    x: 15,
    y: 20,
}; 

console.assert(equal(p1,p1));
console.assert(!equal(p1,p2));


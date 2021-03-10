type List = Cons | null;

type Cons = {
    first: number,
    rest: List, 
};

const list : Cons = {
    first: 10,
    rest: {
        first: 20,
        rest: null,
    },
};
const list2 : Cons = {
    first: 10,
    rest: null,
};
const list3 : Cons = {
    first: 10,
    rest: {
        first: 20,
        rest: {
            first: 30,
            rest: null,
        },
    },
};


function sum(lst: List) : number{
    if (typeof lst !== "object"){
        return 0;
    } else {
        return lst.first + sum(lst.rest);
    }
}


console.assert(sum(list3) === 60);
console.assert(sum(list2) === 10);
console.assert(sum(list) === 30);
console.assert(sum(null) === 0);



type Cons = {
    first: number,
    rest: Cons | null, 
};

type Type = {
    x: number,
    y: AnotherType,
};

type AnotherType = {
    z: number,
};


const list : Cons = {
    first: 10,
    rest: {
        first: 20,
        rest: null,
    },
};

const t : Type = {
    x: 10,
    y: {
        z: 20,
    },
};


//def sum(lst: List) -> int:
//    if isinstance(lst, Empty):
//        return 0
//    else:
//        return lst.first + sum(lst.rest)
//
//assert sum(Empty()) == 0
//assert sum(Cons(10, Empty())) == 10
//assert sum((Cons(40, Cons(10, Empty())))) == 50


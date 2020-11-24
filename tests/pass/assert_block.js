const a = 1;
const b = 8;
const c = 4;
{
    const a  = 10;
    const d = 10;
    {
        const a = 2;
        assert(a + d + c === 16);
    }
    assert(a + b + c === 22);
}

assert(a + b / c === 3);
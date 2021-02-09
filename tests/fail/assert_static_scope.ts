const x = "global";
if(true) {
    function show_x() {
        console.log(x);
    }

    show_x();
    const x = "block";
    show_x();
}

$(document).keyup(function(event) {
    if ($("#q").is(":focus") && (event.key == "Enter")) {
        $("#goButton").click();
    }
});

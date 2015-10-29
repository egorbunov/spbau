uses
    help;
var
    i: longint;
begin
    randseed := 234097;
    createdict(10, 5);
    page(4);
    for i := 1 to 120 do
        par(random(5) + 1);
    dump;
end.
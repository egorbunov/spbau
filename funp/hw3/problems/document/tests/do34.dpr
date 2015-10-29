uses
    help;
var
    i: longint;
begin
    randseed := 234097;
    createdict(100, 5);
    page(10);
    for i := 1 to 40 do
        par(random(10) + 1);
    dump;
end.
uses
    help;
var
    i: longint;
begin
    randseed := 234097;
    createdict(100, 10);
    page(40);
    for i := 1 to 10 do
        par(random(10) + 1);
    dump;
end.
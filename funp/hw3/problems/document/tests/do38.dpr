uses
    help;
var
    i: longint;
begin
    randseed := 239012;
    createdict(1000, 5);
    page(8);
    for i := 1 to 50 do
        par(random(10) + 1);
    dump;
end.
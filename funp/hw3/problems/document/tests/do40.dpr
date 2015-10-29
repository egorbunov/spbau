uses
    help;
var
    i: longint;
begin
    randseed := 239012;
    createdict(1000, 15);
    page(8);
    for i := 1 to 50 do
        par(random(5) + 1);
    dump;
end.
uses
    help;
var
    i: longint;
begin
    randseed := 239012;
    createdict(2, 20);
    page(6);
    for i := 1 to 50 do
        par(random(10) + 1);
    dump;
end.
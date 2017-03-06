uses
    help;
var
    i: longint;
begin
    randseed := 234097;
    createdict(300, 5);
    page(12);
    for i := 1 to 30 do
        par(random(20) + 1);
    dump;
end.
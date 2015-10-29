uses tools;

var
    i: longint; 
begin
    randseed := 5632612;

    n  := 20;

    for i := 2 to n * 2 do begin
        while not addEdge(random(n) + 1, random(n) + 1) do;
    end;

    a1 := a[1];
    b1 := b[1];
    a2 := a[m];
    b2 := b[m];

    findFarest();

    shuffle();
    output();
end.
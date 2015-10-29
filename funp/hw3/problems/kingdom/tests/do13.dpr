uses tools;

var
    i: longint; 
begin
    randseed := 5632613;

    n  := 30;

    a1 := 1;
    b1 := 2;
    a2 := 2;
    b2 := 1;

    for i := 2 to n * 2 do begin
        while not addEdge(random(n div 2) + 1, random(n div 2) + 1) do;
        while not addEdge(random(n div 2) + n div 2 + 1, random(n div 2) + n div 2 + 1) do;
    end;

    findFarest();

    shuffle();
    output();
end.
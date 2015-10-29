uses tools;

var
    i: longint; 
begin
    randseed := 5632620;

    n  := max_n;
    a1 := 1;
    b1 := 2;
    a2 := 2;
    b2 := 1;

    for i := 1 to n - 1 do begin
        addEdge(i, i + 1);
    end;
    addEdge(n - 2, n);

    findFarest();

    shuffle();
    output();
end.
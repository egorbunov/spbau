uses tools;

var
    i: longint; 
begin
    randseed := 5632615;

    n  := max_n;

    a1 := 1;
    b1 := 2;
    a2 := 2;
    b2 := 1;

    for i := 2 to n - 1 do begin
        addEdge(i - 1, i);
    end;
    addEdge(1, n - 1);
    addEdge(n div 2 - 5, n);

    findFarest();

    shuffle();
    output();
end.
uses tools;

var
    i: longint; 
begin
    randseed := 5632617;

    n  := max_n;

    a1 := 1;
    b1 := 2;
    a2 := 2;
    b2 := 1;

    for i := 2 to n do begin
        addEdge(i - 1, i);
    end;
    addEdge(1, n);
    addEdge(n div 2 + 1, n div 2 + 5);
    addEdge(3, n);

    findFarest();

    shuffle();
    output();
end.
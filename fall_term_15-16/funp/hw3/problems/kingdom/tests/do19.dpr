uses tools;

var
    i: longint; 
begin
    randseed := 5632620;

    n  := max_n;

    a1 := 1;
    b1 := 2;
    a2 := 1;
    b2 := n div 2;

    for i := 2 to n do begin
        if random < 0.1 then addEdge(1, i);
    end;
    addEdge(a1, b1);
    addEdge(a2, b2);

    for i := 2 to n - 1 do begin
        addEdge(i, i + 1);
    end;
    addEdge(2, n);

    shuffle();
    output();
end.
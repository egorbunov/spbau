uses tools;

var
    i: longint; 
begin
    randseed := 5632618;

    n  := max_n;

    a1 := 1;
    b1 := 2;
    a2 := 1;
    b2 := n div 2;

    for i := 2 to n do begin
        addEdge(1, i);
    end;
    for i := 2 to n - 1 do begin
        addEdge(i, i + 1);
    end;
    addEdge(2, n);

    shuffle();
    output();
end.
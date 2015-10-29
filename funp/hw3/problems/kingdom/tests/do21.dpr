uses tools;

var
    i: longint; 
begin
    randseed := 5632621;

    n  := max_n;
    a1 := 1;
    b1 := 2;
    a2 := n - 1;
    b2 := n;

    for i := 1 to n div 2 - 1 do begin
        addEdge(2 * i - 1, 2 * i);
        addEdge(2 * i - 1, 2 * i + 1);
        addEdge(2 * i, 2 * i + 2);
    end;
    addEdge(a2, b2);

    shuffle();
    output();
end.
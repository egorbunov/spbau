uses tools;

var
    i: longint; 
begin
    randseed := 5632623;

    n  := max_n;
    a1 := 1;
    b1 := 2;
    a2 := n;
    b2 := n - 1;

    for i := 1 to n div 2 - 1 do begin
        addEdge(2 * i - 1, 2 * i);
        addEdge(2 * i - 1, 2 * i + 1);
        addEdge(2 * i, 2 * i + 2);
    end;
    addEdge(a2, b2);
    addEdge(n div 2 + 1, n div 2 + 6);
    addEdge(n div 3 - 1, n div 3 + 2);

    shuffle();
    output();
end.
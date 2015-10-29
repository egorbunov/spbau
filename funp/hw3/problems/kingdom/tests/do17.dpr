uses tools;

var
    i: longint; 
begin
    randseed := 5632617;

    n  := max_n;

    a1 := 1;
    b1 := 2;
    a2 := n div 2 - 5;
    b2 := n div 2 - 6;

    for i := 2 to n do begin
        addEdge(i - 1, i);
    end;
    addEdge(1, n);
    addEdge(n div 4, n * 3 div 4);

    shuffle();
    output();
end.
uses tools;

var
    i: longint; 
begin
    randseed := 5632614;

    n  := max_n;

    a1 := 1;
    b1 := 2;
    a2 := n div 2 + 2;
    b2 := n div 2 + 3;

    for i := 2 to n do begin
        addEdge(i - 1, i);
    end;
    addEdge(1, n);

    findFarest();

//    shuffle();
    output();
end.
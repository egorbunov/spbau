uses tools;

const
    k1 = 30;
    k2 = 30;

var
    i, j: longint; 
begin
    randseed := 5632627;

    n  := max_n;
    a1 := (n + k1 + k2) div 2 + 1;
    b1 := (n + k1 + k2) div 2;
    a2 := b1;
    b2 := a1;

    for i := 1 to k1 do begin
        for j := i + 1 to k1 do begin
            addEdge(i, j);
        end;
    end;
    addEdge(k1, k1 + 1);

    for i := 1 to k2 do begin
        for j := i + 1 to k2 do begin
            addEdge(k1 + i, k2 + j);
        end;
    end;

    for i := k1 + k2 to n do begin
        addEdge(i, i mod n + 1);
    end;

    shuffle();
    output();
end.
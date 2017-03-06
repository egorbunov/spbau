uses tools;

const
    k = 44;

var
    i, j: longint; 
begin
    randseed := 5632629;

    n  := max_n - 3;
    a1 := (n + k) div 2 + 1;
    b1 := (n + k) div 2;
    a2 := b1;
    b2 := a1;

    for i := 1 to k do begin
        for j := i + 1 to k do begin
            addEdge(i, j);
        end;
    end;

    for i := k to n do begin
        addEdge(i, i mod n + 1);
    end;

    shuffle();
    output();
end.
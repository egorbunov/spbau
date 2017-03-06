uses tools;

const
    k = 10;

var
    i, j: longint; 
begin
    randseed := 5632625;

    n  := max_n;
    a1 := 1;
    b1 := 2;
    a2 := n;
    b2 := n - 1;

    for i := 1 to n div k do begin
        for j := 2 to k - 1 do begin
            addEdge(k * (i - 1) + 1, k * (i - 1) + j);
            addEdge(k * (i - 1) + k, k * (i - 1) + j);
        end;
        if i <> 1 then begin
            addEdge(k * i, (k * i) mod n + 1);
        end;
    end;

    findFarest();

    shuffle();
    output();
end.
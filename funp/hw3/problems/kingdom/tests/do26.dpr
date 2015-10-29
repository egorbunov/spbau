uses tools;

const
    k = 10;

var
    i, j, l: longint; 
begin
    randseed := 5632626;

    n  := max_n;

    for i := 1 to n div k do begin
        for j := 1 to k do begin
            for l := j + 1 to k do begin
                addEdge((i - 1) * k + j, (i - 1) * k + l);
            end;
        end;
        addEdge(i * k, (i * k) mod n + 1);
    end;

    //findFarest();

    shuffle();

    // Hardcoded start/end
    a1 := 65;
    b1 := 59;
    a2 := 68;
    b2 := 48;

    output();
end.
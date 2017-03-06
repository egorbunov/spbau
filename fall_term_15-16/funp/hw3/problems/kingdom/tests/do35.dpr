uses tools;

var
    i: longint; 
begin
    randseed := 5632625;

    n  := 100;

    for i := 1 to max_m do begin
        while not addEdge(random(n) + 1, random(n) + 1) do;
    end;

    //findFarest();

    shuffle();

    // Hardcoded start/end
    a1 := 48;
    b1 := 50;
    a2 := 38;
    b2 := 72;

    output();
end.
{$apptype console}
uses tools;

var
    i: longint;
    d: longint;
begin
    randseed := 5632623;

    n := 100;
    d := 10;

    for i := 1 to max_m do begin
        while not addEdge(
            random(2 * d) + round(i * (n - d) / max_m) - d,
            random(2 * d) + round(i * (n - d) / max_m) - d
        ) do;
    end;

    //findFarest();

    shuffle();

    // Hardcoded start/end 
    a1 := 62;
    b1 := 70;
    a2 := 99;
    b2 := 9;

    output();
end.
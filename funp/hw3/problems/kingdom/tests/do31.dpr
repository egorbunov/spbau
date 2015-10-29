{$apptype console}
uses tools;

var
    i: longint;
    d: longint;
    q: longint;
begin
    randseed := 5632622;

    n := 100;
    d := 5;

    q := n * d * 5 div 6;

    for i := 1 to q do begin
        //writeln(i);
        while not addEdge(
            random(2 * d) - d + round(i * (n - d) / q),
            random(2 * d) - d + round(i * (n - d) / q)
        ) do;
    end;

    //findFarest();
    
    shuffle();

    // Hardcoded start/end
    a1 := 55;
    b1 := 78;
    a2 := 54;
    b2 := 24;

    output();
end.
{$apptype console}
uses tools;

var
    i: longint;
    d: longint;
    q: longint;
begin
    randseed := 5632622;

    n := 100;
    d := 3;

    q := n * d * 5 div 6;

    for i := 1 to q do begin
        //writeln(i);
        while not addEdge(
            random(2 * d) - d + round(i * (n - d) / q),
            random(2 * d) - d + round(i * (n - d) / q)
        ) do;
    end;
    a1 := a[1];
    b1 := b[1];
    a2 := a[m];
    b2 := b[m];

    findFarest();

    shuffle();
    output();
end.
{$apptype console}
uses tools;

var
    i: longint;
    d: longint;
    q: longint;
begin
    randseed := 5632630;

    n := 100;
    d := 2;

    q := n * d * 52 div 80;

    for i := 1 to q do begin
        //writeln(i);
        while not addEdge(
            random(2 * d) - d + round(i * n / q),
            random(2 * d) - d + round(i * n / q)
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
{$apptype console}
uses tools;

var
    i: longint;
    d: longint;
begin
    randseed := 56326241;

    n := 100;
    d := 7;

    for i := 1 to 996 do begin
        while not addEdge(
            random(2 * d) + round(i * (n - d) / max_m) - d div 2,
            random(2 * d) + round(i * (n - d) / max_m) - d div 2
        ) do;
    end;
    a1 := a[1];
    b1 := b[1];
    a2 := a[m];
    b2 := b[m];

    shuffle();
    output();
end.
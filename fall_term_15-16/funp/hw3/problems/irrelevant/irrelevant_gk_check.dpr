{$q+,r+}
const
    max_n = 2000;
    max_m = 1000000000;

var
    n, m, i, j: longint;
    c: array [0..max_n, 0..max_n] of longint;
begin
    assign(input, 'irrelevant.in'); reset(input);
    assign(output, 'irrelevant.out'); rewrite(output);

    read(n, m);
    assert((1 <= n) and (n <= max_n), 'N out of bounds');
    assert((2 <= m) and (n <= max_m), 'M out of bounds');

    dec(n);

    c[0][0] := 1;
    for i := 1 to n do begin
        c[i][0] := 1;
        for j := 1 to i do begin
            c[i][j] := (c[i - 1][j] + c[i - 1][j - 1]) mod m;
        end;
    end;

    j := 0;
    for i := 0 to n do begin
        if c[n][i] = 0 then begin
            inc(j);
        end;
    end;
    writeln(j);

    for i := 0 to n do begin
        if c[n][i] = 0 then begin
            writeln(i + 1, ' ');
        end;
    end;

    close(input);
    close(output);
end.
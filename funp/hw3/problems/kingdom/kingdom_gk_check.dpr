{$q+,r+,o-}
uses
    sysutils;

const
    max_n = 100;
    max_m = 1000;

var
    i, j, n, m: longint;
    a1, a2, b1, b2, e1, e2: longint;
    v1, v2: array [1..2 * max_m] of longint;
    a: array[1..max_n, 1..max_n] of longint;
    d, p: array[1..2 * max_m] of longint;
    u: array[1..2 * max_m] of boolean;

procedure o(e, l: longint);
begin
    if p[e] = 0 then begin
        writeln(d[e2], ' ', l);
    end else begin
        o(p[e], l + 1);
    end;
    writeln(v1[e], ' ', v2[e]);
end;

begin
    assign(input, 'kingdom.in'); reset(input);
    assign(output, 'kingdom.out'); rewrite(output);

    read(n, m, a1, b1, a2, b2);

    assert((0 < n) and (n <= max_n), 'N out of bounds');
    assert((0 < m) and (m <= max_m), 'M out of bounds');
    assert((a1 <> a2) or (a2 <> b2), 'Target and destination coinside');

    for i := 1 to m do begin
        read(v1[i], v2[i]);

        assert((0 < v1[i]) and (v1[i] <= n), format('Portal %d: Town 1 out of bounds', [i]));
        assert((0 < v2[i]) and (v2[i] <= n), format('Portal %d: Town 2 out of bounds', [i]));
        assert(v1[i] <> v2[i], format('Portal %d: Towns coinside', [i]));
        assert(a[v1[i]][v2[i]] = 0, format('Portal %d: Duplicate edge %d', [i, abs(a[v1[i]][v2[i]])]));

        a[v1[i]][v2[i]] := i;
        a[v2[i]][v1[i]] := i + m;
        v1[i + m] := v2[i];
        v2[i + m] := v1[i];
    end;

    e1 := a[a1][b1];
    e2 := a[a2][b2];
    assert(e1 <> 0, 'A1 and B1 are not connected');
    assert(e2 <> 0, 'A2 and B2 are not connected');

    for i := 1 to 2 * m do begin
        u[i] := false;
        d[i] := maxlongint;
    end;
    d[e1] := 0;

    repeat
        j := 0;
        for i := 1 to 2 * m do begin
            if not u[i] and ((j = 0) or (d[i] < d[j])) then begin
                j := i;
            end;
        end;
        assert(d[j] < maxlongint, 'Path does not exists');
        u[j] := true;

        for i := 1 to 2 * m do begin
            if abs(i - j) = m then continue;
            if
                (((v1[j] = v1[i]) and (a[v2[j]][v2[i]] <> 0)) or
                ((v2[j] = v2[i]) and (a[v1[j]][v1[i]] <> 0))) and
                (d[i] > d[j] + 1)
            then begin
                d[i] := d[j] + 1;
                p[i] := j;
            end else if (a[v1[j]][v1[i]] <> 0) and (a[v2[j]][v2[i]] <> 0) and (d[i] > d[j] + 2) then begin
                d[i] := d[j] + 2;
                p[i] := j;
            end;
        end;
    until j = e2;

    o(e2, 1);

    close(input);
    close(output);
end.
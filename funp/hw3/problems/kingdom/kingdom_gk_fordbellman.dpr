uses
    sysutils;
const
    max_n = 100;
    max_m = 1000;

type
    trow = array [1..2 * max_m] of word;

var
    i, j, n, m: word;
    a1, a2, b1, b2, e1, e2: word;
    v1, v2: array [1..2 * max_m] of word;
    a: array[1..max_n, 1..max_n] of word;
    d, p: array[1..2 * max_m] of word;
    u: array[1..2 * max_m] of boolean;
    z1, z2: array [1..2 * max_m] of trow;
    cz1, cz2: array [1..2 * max_m] of word;

procedure o(e, l: word);
begin
    if p[e] = 0 then begin
        writeln(d[e2], ' ', l);
    end else begin
        o(p[e], l + 1);
    end;
    writeln(v1[e], ' ', v2[e]);
end;

var
    f: boolean;

procedure update(cd: word; var z1, z2: trow);
var
    j: word;
    k: word;
begin
    inc(cd);
    for j := 1 to cz1[i] do begin
        k := z1[j];
        if (d[k] > cd) then begin
            d[k] := cd;
            p[k] := i;
            f := true;
        end;
    end;
    inc(cd);
    for j := 1 to cz2[i] do begin
        k := z2[j];
        if (d[k] > cd) then begin
            d[k] := cd;
            p[k] := i;
            f := true;
        end;
    end;
end;

procedure swap(var a, b: word);
var
    t: word;
begin
    t := a; a := b; b := t;
end;

const
    maxword = 32000;

begin
    assign(input, 'kingdom.in'); reset(input);
    assign(output, 'kingdom.out'); rewrite(output);

    read(n, m, a1, b1, a2, b2);

    for i := 1 to m do begin
        read(v1[i], v2[i]);
        a[v1[i]][v2[i]] := i;
        a[v2[i]][v1[i]] := i + m;
        v1[i + m] := v2[i];
        v2[i + m] := v1[i];
    end;

    e1 := a[a1][b1];
    e2 := a[a2][b2];

    for i := 1 to 2 * m do begin
        u[i] := false;
        d[i] := maxword div 2;
    end;
    d[e1] := 0;

    for i := 1 to 2 * m do begin
        for j := 1 to 2 * m do begin
            if abs(i - j) = m then continue;
            if ((v1[j] = v1[i]) and (a[v2[j]][v2[i]] <> 0)) or
               ((v2[j] = v2[i]) and (a[v1[j]][v1[i]] <> 0))
            then begin
                inc(cz1[i]);
                z1[i][cz1[i]] := j;
            end else if (a[v1[j]][v1[i]] <> 0) and (a[v2[j]][v2[i]] <> 0) then begin
                inc(cz2[i]);
                z2[i][cz2[i]] := j;
            end;
        end;
        for j := 1 to cz1[i] do swap(z1[i][j], z1[i][random(j) + 1]);
        for j := 1 to cz2[i] do swap(z2[i][j], z2[i][random(j) + 1]);
    end;

    repeat
        f := false;
        for i := 1 to 2 * m do begin
            if d[i] < maxword div 2 then begin
                update(d[i], z1[i], z2[i]);
            end;
        end;
    until not f;

    o(e2, 1);

    close(input);
    close(output);
end.
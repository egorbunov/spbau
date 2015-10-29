{$q+,r+}
unit tools;


interface

const
    max_m = 1000;
    max_n = 100;
    useShuffle = true;

var
    n, m, a1, b1, a2, b2: longint;
    a, b: array [1..2 * max_m] of longint;
    c: array [1..max_n, 1..max_n] of longint;

function addEdge(v1, v2: longint): boolean;
procedure shuffle();
procedure output();
procedure findFarest();

implementation

function addEdge(v1, v2: longint): boolean;
begin
    addEdge := (0 < v1) and (v1 <= n) and (0 < v2) and (v2 <= n) and (c[v1][v2] = 0);
    if result then begin
        inc(m);
        a[m] := v1;
        b[m] := v2;
        c[v1][v2] := m;
        c[v2][v1] := m;
    end;
end;

procedure swap(var a, b: longint);
var
    t: longint;
begin
    t := a; a := b; b := t;
end;

procedure shuffle();
var
    i, j, k: longint;
begin
    if useShuffle then begin
        for i := 1 to n do begin
            k := random(i) + 1;
            for j := 1 to m do begin
                if a[j] = i then a[j] := k else if a[j] = k then a[j] := i;
                if b[j] = i then b[j] := k else if b[j] = k then b[j] := i;
            end;
            if a1 = i then a1 := k else if a1 = k then a1 := i;
            if b1 = i then b1 := k else if b1 = k then b1 := i;
            if a2 = i then a2 := k else if a2 = k then a2 := i;
            if b2 = i then b2 := k else if b2 = k then b2 := i;
        end;
        for i := 1 to m do begin
            if random > 0.5 then swap(a[i], b[i]);
            k := random(i) + 1;
            swap(a[i], a[k]);
            swap(b[i], b[k]);
        end;
    end;
end;

procedure output();
var
    i: longint;
begin
    writeln(n, ' ', m, ' ', a1, ' ', b1, ' ', a2, ' ', b2);
    for i := 1 to m do begin
        writeln(a[i], ' ', b[i]);
    end;
end;

var
    q: array [1..2 * max_m, 1..2 * max_m] of longint;
    z, l: array [1..2 * max_m, 1..2 * max_m] of longint;
    cz: array [1..2 * max_m] of longint;

procedure findFarest();
var
    i, j: longint;
    c: array [1..max_n, 1..max_n] of longint;
    e1, e2: longint;
    d: array[1..2 * max_m] of longint;
    u: array[1..2 * max_m] of boolean;
    me1, me2, md: longint;
begin
    fillchar(c, sizeof(c), 0);
    for i := 1 to m do begin
        c[a[i]][b[i]] := i;
        c[b[i]][a[i]] := i + m;
        a[i + m] := b[i];
        b[i + m] := a[i];
    end;

    fillchar(cz, sizeof(cz), 0);
    
    for i := 1 to 2 * m do begin
        for j := 1 to 2 * m do begin
            if abs(i - j) = m then begin
                q[i][j] := maxlongint div 2;
                continue;
            end;
            if ((a[j] = a[i]) and (c[b[j]][b[i]] <> 0)) or
               ((b[j] = b[i]) and (c[a[j]][a[i]] <> 0))
            then begin
                inc(cz[i]);
                z[i][cz[i]] := j;
                l[i][cz[i]] := 1;
                q[i][j] := 1;
            end else if (c[a[j]][a[i]] <> 0) and (c[b[j]][b[i]] <> 0) then begin
                inc(cz[i]);
                z[i][cz[i]] := j;
                l[i][cz[i]] := 2;
                q[i][j] := 2;
            end else begin
                q[i][j] := maxlongint div 2;
            end;
        end;
    end;

    md := 0;
    me1 := 0;
    me2 := 0;
    for e1 := 1 to m do begin
        e2 := 0;
        for i := 1 to 2 * m do begin
            u[i] := false;
            d[i] := maxlongint div 2;
        end;
        d[e1] := 0;

        while true do begin
            j := 0;
            for i := 1 to 2 * m do begin
                if not u[i] and ((j = 0) or (d[i] < d[j])) then begin
                    j := i;
                end;
            end;
            if (j = 0) or (d[j] >= maxlongint div 2) then begin
                break;
            end;
            e2 := j;
            u[j] := true;

            {
            for i := 1 to 2 * m do begin
                if abs(i - j) = m then continue;
                if (d[i] > d[j] + q[j][i]) then begin
                    d[i] := d[j] + q[j][i];
                end;
            end;
            }

            for i := 1 to cz[j] do begin
                if (d[z[j][i]] > d[j] + l[j][i]) then begin
                    d[z[j][i]] := d[j] + l[j][i];
                end;
            end;
        end;
        if md < d[e2] then begin
            me1 := e1;
            me2 := e2;
            md := d[e2];
        end;
    end;
    a1 := a[me1];
    b1 := b[me1];
    a2 := a[me2];
    b2 := b[me2];
    //writeln(me1, ' ', me2, ' ', md);
end;

var
    i: longint;
begin
    m := 0;
    fillchar(c, sizeof(c), 0);
    for i := 1 to max_n do begin
        c[i][i] := -1;
    end;
end.

const
    max_n = 10;
type
    tpoly = record
        sx, sy: longint;
        a: array [1..max_n + 1] of longint;
    end;

var
    a: array[1..max_n, 1..max_n, 1..max_n] of array of tpoly;
    c: array[1..max_n, 1..max_n, 1..max_n] of longint;

function transpose(var p: tPoly): tPoly;
var
    i, j: longint;
begin
    result.sy := p.sx;
    result.sx := p.sy;
    for j := 1 to p.sx do begin
        result.a[j] := 0;
        for i := 1 to p.sy do begin
            result.a[j] := result.a[j] + ((p.a[i] shr (j - 1)) and 1) shl (i - 1);
        end;
    end;
end;

function mirrorX(var p: tPoly): tPoly;
var
    i, j: longint;
begin
    result.sx := p.sx;
    result.sy := p.sy;
    for i := 1 to p.sy do begin
        result.a[i] := 0;
        for j := 1 to p.sx do begin
            result.a[i] := result.a[i] + ((p.a[i] shr (j - 1)) and 1) shl (p.sx - j);
        end;
    end;
end;

function cmp(var p1, p2: tPoly): longint;
var
    i: longint;
begin
    if (p1.sy <> p2.sy) then begin
        result := p1.sy - p2.sy;
    end else if (p1.sx <> p2.sx) then begin
        result := p1.sx - p2.sx;
    end else begin
        for i := 1 to p1.sy do begin
            if p1.a[i] <> p2.a[i] then begin
                result := p1.a[i] - p2.a[i];
                exit;
            end;
        end;
        result := 0
    end;
end;

function mirrorY(var p: tPoly): tPoly;
var
    i, j: longint;
begin
    result.sy := p.sy;
    result.sx := p.sx;
    j := p.sy;
    for i := 1 to p.sy do begin
        result.a[i] := p.a[j];
        dec(j);
    end;
end;

procedure min(var p: tPoly);
var
    p2: tPoly;
begin
    p2 := mirrorY(p);
    if cmp(p, p2) > 0 then p := p2;
    p2 := mirrorX(p2);
    if cmp(p, p2) > 0 then p := p2;
    p2 := mirrorY(p2);
    if cmp(p, p2) > 0 then p := p2;
end;

procedure add(l: longint; var p: tPoly);
var
    p2: tPoly;
    i: longint;
begin
    if p.sx <> p.sy then begin
        if p.sy > p.sx then p := transpose(p);
        min(p);
    end else begin
        p2 := transpose(p);
        min(p2);
        min(p);
        if cmp(p, p2) > 0 then p := p2;
    end;

    for i := 1 to c[l][p.sy][p.sx] do begin
        if cmp(p, a[l][p.sy][p.sx][i]) = 0 then exit;
    end;

    inc(c[l][p.sy][p.sx]);
    if c[l][p.sy][p.sx] >= length(a[l][p.sy][p.sx]) then begin
        setLength(a[l][p.sy][p.sx], c[l][p.sy][p.sx] * 2);
    end;
    a[l][p.sy][p.sx][c[l][p.sy][p.sx]] := p;
end;

const
    dx: array [1..4] of longint = (1, -1, 0,  0);
    dy: array [1..4] of longint = (0,  0, 1, -1);
var
    p, pn: tPoly;
    i, j, d, y, x, ty, nx, ny: longint;
    zx, zy: longint;
    s: longint;
    n: longint;
    sx, sy: longint;
    t: longint;
begin
    assign(input, 'lattice.in'); reset(input);
    assign(output, 'lattice.out'); rewrite(output);

    read(n, sx, sy);
    if sy > sx then begin
        t := sy; sy := sx; sx := t;
    end;

    p.sx := 1;
    p.sy := 1;
    p.a[1] := 1;

    add(1, p);

    for i := 2 to n do begin
        for zx := 1 to i do for zy := 1 to i do for j := 1 to c[i - 1][zy][zx] do begin
            p := a[i - 1][zy][zx][j];
            for y := 1 to p.sy do begin
                for x := 1 to p.sx do begin
                    if (p.a[y] shr (x - 1)) and 1 = 1 then begin
                        for d := 1 to 4 do begin
                            nx := x + dx[d];
                            ny := y + dy[d];
                            if (ny < 1) then begin
                                for ty := p.sy downto 1 do begin
                                    pn.a[ty + 1] := p.a[ty];
                                end;
                                pn.sx := p.sx;
                                pn.sy := p.sy + 1;
                                pn.a[1] := 1 shl (nx - 1);
                                add(i, pn);
                            end else if ny > p.sy then begin
                                pn := p;
                                pn.sy := ny;
                                pn.a[ny] := 1 shl (nx - 1);
                                add(i, pn);
                            end else if nx < 1 then begin
                                for ty := 1 to p.sy do begin
                                    pn.a[ty] := p.a[ty] shl 1;
                                end;
                                pn.sy := p.sy;
                                pn.sx := p.sx + 1;
                                pn.a[ny] := pn.a[ny] + 1;
                                add(i, pn);
                            end else if nx > p.sx then begin
                                pn := p;
                                pn.sx := p.sx + 1;
                                pn.a[ny] := pn.a[ny] + (1 shl (nx - 1));
                                add(i, pn);
                            end else begin
                                if (p.a[ny] shr (nx - 1)) and 1 = 0 then begin
                                    pn := p;
                                    pn.a[ny] := pn.a[ny] + (1 shl (nx - 1));
                                    add(i, pn);
                                end;
                            end;
                        end;
                    end;
                end;
            end;
        end;
    end;

    s := 0;
    for zx := 1 to sx do begin
        for zy := 1 to sy do begin
            s := s + c[n][zy][zx];
        end;
    end;
    writeln(s);

    close(input);
    close(output);
end.
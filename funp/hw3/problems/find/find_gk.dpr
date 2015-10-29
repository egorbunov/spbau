{$q+,r+,o-}
uses
    math;

const
    max_n = 100;
    eps   = 1e-7;

function between(a, b, x: extended): boolean;
begin
    between := 
        (a - eps < x) and (x < b + eps) or
        (b - eps < x) and (x < a + eps);
end;

function same(x1, y1, x2, y2: extended): boolean;
begin
    same := (abs(x1 - x2) < eps) and (abs(y1 - y2) < eps);
end;

function angle(dx, dy, a: extended): extended;
begin
    result := arctan2(dy, dx) - a;
    while result > pi + eps do begin
        result := result - 2 * pi;
    end;
    while result < - pi + eps do begin
        result := result + 2 * pi;
    end;
end;

function intersect(a1, b1, c1, a2, b2, c2: extended; var x, y: extended): boolean;
var
    d: extended;
begin
    d := a1 * b2 - a2 * b1;
    result := abs(d) > eps;
    if result then begin
        x := (c1 * b2 - c2 * b1) / d;
        y := (a1 * c2 - a2 * c1) / d;
    end;
end;

var
    i, n: longint;
    x, y: array[1..max_n + 1] of longint;
    a, b, c: array[1..max_n] of longint;
    j: longint;
    ang, ma, cx, cy, ca, cb, cc, xx, xy, d, md, nx, ny: extended;
    bx, by: array [1..max_n * 4] of extended;
    bc: longint;
begin
    assign(input, 'find.in'); reset(input);
    assign(output, 'find.out'); rewrite(output);

    read(n);

    for i := 1 to n do begin
        read(x[i], y[i]);
    end;
    x[n + 1] := x[1];
    y[n + 1] := y[1];

    for i := 1 to n do begin
        a[i] := y[i + 1] - y[i];
        b[i] := x[i] - x[i + 1];
        c[i] := a[i] * x[i] + b[i] * y[i];
    end;

    j := 1;
    for i := 2 to n do begin
        if (x[j] > x[i]) or (x[j] = x[i]) and (y[j] > y[i]) then begin
            j := i;
        end;
    end;

    bc := 0;
    cx := x[j];
    cy := y[j];
    ang := 0;
    repeat
        inc(bc);
        bx[bc] := cx;
        by[bc] := cy;
        ma := pi;
        for i := 1 to n do begin
            if abs(a[i] * cx + b[i] * cy - c[i]) < eps then begin
                if between(x[i], x[i + 1], cx) and between(y[i], y[i + 1], cy) then begin
                    if not same(cx, cy, x[i], y[i]) then begin
                        ca := angle(x[i] - x[i + 1], y[i] - y[i + 1], ang);
                        if ma > ca then begin
                            ma := ca;
                        end;
                    end;
                    if not same(cx, cy, x[i + 1], y[i + 1]) then begin
                        ca := angle(x[i + 1] - x[i], y[i + 1] - y[i], ang);
                        if ma > ca then begin
                            ma := ca;
                        end;
                    end;
                end;
            end;
        end;
        ang := ang + ma;

        ca := -sin(ang);
        cb := cos(ang);
        cc := ca * cx + cb * cy;
        md := 1e10;
        nx := cx;
        ny := cy;
        for i := 1 to n do begin
            if intersect(ca, cb, cc, a[i], b[i], c[i], xx, xy) then begin
                if between(x[i], x[i + 1], xx) and between(y[i], y[i + 1], xy) then begin
                    d := (xx - cx) * cb - (xy - cy) * ca;
                    if (d > eps) and (d < md - eps) then begin
                        md := d;
                        nx := xx;
                        ny := xy;
                    end;
                end;
            end;
        end;
        ang := arctan2(ny - cy, nx - cx);
        cx := nx;
        cy := ny;
    until (abs(cx - bx[1]) < eps) and (abs(cy - by[1]) < eps);

    writeln(bc);
    for i := 1 to bc do begin
        writeln(bx[i]:0:4, ' ', by[i]:0:4);
    end;

    close(input);
    close(output);
end.
{$q+,r+,o-}
uses
    sysutils;

const
    max_n = 100;
    max_c = 999;

function between(a, b, x: longint): boolean;
begin
    between := (a <= x) and (x <= b);
end;

var
    i, j, n: longint;
    x1, y1, x2, y2, z: array[1..max_n] of longint;
    ax, ay: array[1..max_n] of extended;
    ax0: extended;
    f1, f2: boolean;

function checkX(x_1, z_1, x_2, z_2: longint): boolean;
var
    i: longint;
begin
    result := true;
    ax0 := x_1 - (x_2 - x_1) * z_1 / (z_2 - z_1);
    for i := 1 to n do begin
        result := result and between(x1[i] * (z_2 - z_1), x2[i] * (z_2 - z_1), x_1 * (z_2 - z_1) + (x_2 - x_1) * (z[i] - z_1));
        ax[i] := x_1 + (x_2 - x_1) * (z[i] - z_1) / (z_2 - z_1);
    end;
end;

begin
    assign(input, 'gunman.in'); reset(input);
    assign(output, 'gunman.out'); rewrite(output);

    read(n);
    assert((2 <= n) and (n <= max_n), 'N out of bounds');

    for i := 1 to n do begin
        read(x1[i], y1[i], x2[i], y2[i], z[i]);
        assert((0 < x1[i]) and (x1[i] <= max_c), format('X1[%d] out of bounds', [i]));
        assert((0 < y1[i]) and (y1[i] <= max_c), format('Y1[%d] out of bounds', [i]));
        assert((0 < x2[i]) and (x2[i] <= max_c), format('X2[%d] out of bounds', [i]));
        assert((0 < y2[i]) and (y2[i] <= max_c), format('Y2[%d] out of bounds', [i]));
        assert((0 <  z[i]) and ( z[i] <= max_c), format(' Z[%d] out of bounds', [i]));

        assert(x1[i] < x2[i], format('X1[%d] >= X2[%d]', [i, i]));
        assert(y1[i] < y2[i], format('Y1[%d] >= Y2[%d]', [i, i]));
        assert((i = 1) or (z[i - 1] < z[i]), format('Z[%d] >= Z[%d]', [i - 1, i]));
    end;

    f1 := false;
    for i := 1 to n do begin
        f1 := true;
        for j := 1 to n do begin
            f1 := f1 and between(y1[j] * z[i], y2[j] * z[i], y1[i] * z[j]);
            ay[j] := y1[i] * z[j] / z[i];
        end;
        if f1 then break;
    end;

    f2 := false;
    for i := 1 to n do begin
        for j := i + 1 to n do begin
            f2 := 
                checkX(x1[i], z[i], x1[j], z[j]) or
                checkX(x1[i], z[i], x2[j], z[j]) or
                checkX(x2[i], z[i], x1[j], z[j]) or
                checkX(x2[i], z[i], x2[j], z[j]);
            if f2 then break;
        end;
        if f2 then break;
    end;

    if f1 and f2 then begin
        writeln('SOLUTION');
        writeln(ax0);
        for i := 1 to n do begin
            writeln(ax[i]:0:6, ' ', ay[i]:0:6, ' ', z[i]:0);
        end;
    end else begin
        writeln('UNSOLVABLE');
    end;


    close(input);
    close(output);
end.
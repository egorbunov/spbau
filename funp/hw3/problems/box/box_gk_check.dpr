{$q+,r+,o-}
uses
    sysutils;

const
    max_s = 10000;

procedure swap(var a, b: longint);
var
    t: longint;
begin
    t := a; a := b; b := t;
end;

var
    w, h: array [1..6] of longint;
    i, j: longint;
    f: boolean;
begin
    assign(input, 'box.in'); reset(input);
    assign(output, 'box.out'); rewrite(output);

    for i := 1 to 6 do begin
        read(w[i], h[i]);
        assert((0 < w[i]) and (w[i] <= max_s), format('W[%d] out of bounds', [i]));
        assert((0 < h[i]) and (h[i] <= max_s), format('H[%d] out of bounds', [i]));
        if w[i] > h[i] then swap(w[i], h[i]);
    end;

    for i := 1 to 6 do begin
        for j := i + 1 to 6 do begin
            if (w[i] > w[j]) or (w[i] = w[j]) and (h[i] > h[j]) then begin
                swap(w[i], w[j]);
                swap(h[i], h[j]);
            end;
        end;
    end;

    f := true;

    for i := 1 to 3 do begin
        f := f and (w[2 * i - 1] = w[2 * i]) and (h[2 * i - 1] = h[2 * i]);
    end;

    f := f and (w[1] = w[3]) and (h[3] = h[5]) and (h[1] = w[5]);

    if f then begin
        writeln('POSSIBLE');
    end else begin
        writeln('IMPOSSIBLE');
    end;


    close(input);
    close(output);
end.
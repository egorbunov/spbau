{$o-,q+,r+}
uses
    math;

const
    max_n = 1000;
var
    i, j, k, n, c, t: longint;
    a, b, x: array [0..max_n + 1] of longint;
    u: array [1..max_n] of boolean;
    q, p, z: array [0..max_n] of longint;

procedure rec(i: longint);
var
    j: longint;
begin
    for j := 1 to n do begin
        if (a[j] = p[i] + 1) and (b[j] = i) and (z[i] > 0) then begin
            dec(z[i]);
            u[x[j]] := true;
        end;
    end;
    if p[i] <> 0 then rec(p[i]);
end;

begin
    assign(input, 'joke.in'); reset(input);
    assign(output, 'joke.out'); rewrite(output);

    read(n);

    for i := 1 to n do begin
        read(a[i], b[i]);
        b[i] := n - b[i];
        a[i] := a[i] + 1;
        x[i] := i;
    end;

    for i := 1 to n do begin
        for j := i + 1 to n do begin
            if (b[i] > b[j]) or (b[i] = b[j]) and (a[i] > a[j]) then begin
                t := a[i]; a[i] := a[j]; a[j] := t;
                t := b[i]; b[i] := b[j]; b[j] := t;
                t := x[i]; x[i] := x[j]; x[j] := t;
            end;
        end;
    end;

    a[n + 1] := n + 1;
    b[n + 1] := n + 1;
    q[0] := 0;
    j := 1;
    for i := 1 to n do begin
        q[i] := q[i - 1];
        p[i] := i - 1;
        while b[j] < i do inc(j);
        while b[j] = i do begin
            k := j;
            while (b[j] = i) and (a[j] = a[k]) do begin
                inc(j);
            end;
            c := min(j - k, b[k] - a[k] + 1);
            if q[i] < c + q[a[k] - 1] then begin
                q[i] := c + q[a[k] - 1];
                p[i] := a[k] - 1;
                z[i] := c;
            end;
        end;
    end;

    writeln(n - q[n]);
    rec(n);

    for i := 1 to n do begin
        if not u[i] then write(i, ' ');
    end;

    close(input);
    close(output);
end.

program irrelevant;

{$Q+,R+}
function count(m, i: longint): longint;
var
    r: longint;
begin
    r := 0;
    while m mod i = 0 do begin
        m := m div i;
        inc(r);
    end;
    count := r;
end;

var
    n, m, i, j, c, t, ans: longint;
    p, q, cp: array [1..30] of longint;

begin
    reset(input, 'irrelevant.in');
    rewrite(output, 'irrelevant.out');
    read(n, m);
    n := n - 1;

    i := 2;
    c := 0;
    while i * i <= m do begin
        if m mod i = 0 then begin
            inc(c);
            p[c] := i;
            q[c] := 0;
            while m mod i = 0 do begin
                m := m div i;
                inc(q[c]);
            end;
        end;
        inc(i);
    end;
    if m > 1 then begin
        inc(c);
        p[c] := m;
        q[c] := 1;
    end;

    for i := 1 to c do begin    
        cp[i] := 0;
    end;

    ans := 0;
    for i := 0 to n - 1 do begin
        t := 0;
        for j := 1 to c do begin
            cp[j] := cp[j] + count(n - i, p[j]) - count(i + 1, p[j]);
            assert(cp[j] >= 0);
            if cp[j] >= q[j] then
                inc(t);
        end;
        if t = c then
            inc(ans);
    end;
    writeln(ans);
    for i := 1 to c do begin    
        cp[i] := 0;
    end;

    for i := 0 to n - 1 do begin
        t := 0;
        for j := 1 to c do begin
            cp[j] := cp[j] + count(n - i, p[j]) - count(i + 1, p[j]);
            assert(cp[j] >= 0);
            if cp[j] >= q[j] then
                inc(t);
        end;
        if t = c then
            writeln(i + 2);
    end;
end.

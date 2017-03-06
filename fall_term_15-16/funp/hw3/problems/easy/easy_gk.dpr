const
    max_k = 10000;

function sign(a: longint): longint;
begin
    if a = 0 then sign := 0 else if a < 0 then sign := -1 else sign := 1;
end;

var
    i, n, m, k: longint;
    p: array [1..max_k] of longint;
    tp, tn: longint;
    sn, sm: longint;
    q: extended;
begin
    assign(input, 'easy.in'); reset(input);
    assign(output, 'easy.out'); rewrite(output);

    read(m, n, k);

    for i := 1 to k do begin
        read(q);
        p[i] := round(q * 100);
    end;

    sn := 0;
    for i := 1 to n do begin
        sn := sn + p[i];
    end;

    sm := 0;
    for i := n - m + 1 to n do begin
        sm := sm + p[i];
    end;

    tp := 0;
    for i := n to k do begin
        tn := sign(sn * m - sm * n);
        if tn * tp <= 0 then begin
            if tn > 0 then write('SELL') else write('BUY');
            writeln(' ON DAY ', i);
        end;
        sn := sn + p[i + 1] - p[i - n + 1];
        sm := sm + p[i + 1] - p[i - m + 1];
        tp := tn;
    end;

    close(input);
    close(output);
end.
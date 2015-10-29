{$r+,q+,o-}
uses
    sysutils;
const
    max_n = 100;
    max_k = 10000;
    max_p = 9999;

function sign(a: longint): longint;
begin
    if a = 0 then sign := 0 else if a < 0 then sign := -1 else sign := 1;
end;

var
    i, j, n, m, k: longint;
    p: array [1..max_k] of longint;
    tp, tn: longint;
    sn, sm: longint;
    q: extended;
begin
    assign(input, 'easy.in'); reset(input);
    assign(output, 'easy.out'); rewrite(output);

    read(m, n, k);
    assert((0 < k) and (n < k), 'K out of bounds');
    assert((0 < n) and (n <= max_n) and (n < k), 'N out of bounds');
    assert((0 < m) and (m < n), 'M out of bounds');

    for i := 1 to k do begin
        read(q);
        p[i] := round(q * 100);
        assert((0 < p[i]) and (p[i] <= max_p), format('p[%d] out of bounds', [i]));
    end;

    tp := 0;
    for i := n to k do begin
        sn := 0;
        for j := i - n + 1 to i do begin
            sn := sn + p[j];
        end;
        sm := 0;
        for j := i - m + 1 to i do begin
            sm := sm + p[j];
        end;
        tn := sign(sn * m - sm * n);
        assert(tn <> 0, format('Pm = Pn on day %d', [i]));
        if tn * tp <= 0 then begin
            if tn > 0 then write('SELL') else write('BUY');
            writeln(' ON DAY ', i);
        end;
        tp := tn;
    end;

    close(input);
    close(output);
end.
const
    max_n = 50000;

var
    h: array [1..max_n] of longint;
    i, j, n, t: longint;
begin
    assign(input, 'heapsort.in'); reset(input);
    assign(output, 'heapsort.out'); rewrite(output);

    read(n);

    for i := 1 to n - 1 do begin
        h[i] := i + 1;
        j := i;
        while (j > 1) do begin
            t := h[j]; h[j] := h[j div 2]; h[j div 2] := t;
            j := j div 2;
        end;
    end;
    h[n] := 1;

    for i := 1 to n do begin
        write(h[i], ' ');
    end;

    close(input);
    close(output);
end.
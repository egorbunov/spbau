program heapsort;
const
    maxn = 100000;
var
    i, j, n: longint;
    t: longint;
    a: array [1..maxn] of longint;
begin
    reset(input, 'heapsort.in');
    rewrite(output, 'heapsort.out');

    read(n);
    a[1] := 1;
    for i := 2 to n do begin
        a[i] := a[i - 1];
        a[i - 1] := i;
        j := i - 1;
        while j > 1 do begin
            t := a[j]; a[j] := a[j div 2]; a[j div 2] := t;
            j := j div 2;
        end;
    end;

    for i := 1 to n do begin
        write(a[i], ' ');
    end;
    writeln;

    close(input);
    close(output);
end.
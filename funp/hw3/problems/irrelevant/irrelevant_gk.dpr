{$q+,r+,o-}
const
    max_n = 100000;
    max_s = 32000;
var
    n, m, i, j: longint;
    u: array [2..max_s] of boolean;
    f, e, p: array[1..max_s] of longint;
    cf: longint;
    flag: boolean;
    a: array [1..max_n] of longint;
    c: longint;

procedure add(i, k: longint);
begin
    while i mod f[k] = 0 do begin
        i := i div f[k];
        inc(p[k]);
    end;
end;

procedure sub(i, k: longint);
begin
    while i mod f[k] = 0 do begin
        i := i div f[k];
        dec(p[k]);
    end;
end;

begin
    assign(input, 'irrelevant.in'); reset(input);
    assign(output, 'irrelevant.out'); rewrite(output);

    read(n, m);
    dec(n);

    cf := 0;
    for i := 2 to max_s do begin
        if not u[i] then begin
            for j := 1 to max_s div i do begin
                u[j * i] := true;
            end;
            if m mod i = 0 then begin
                inc(cf);
                f[cf] := i;
                e[cf] := 0;
                while m mod i = 0 do begin
                    m := m div i;
                    inc(e[cf]);
                end;
            end;
        end;
    end;
    if m <> 1 then begin
        inc(cf);
        f[cf] := m;
        e[cf] := 1;
    end;

    c := 0;
    for i := 1 to n do begin
        flag := true;
        for j := 1 to cf do begin
            add(n - i + 1, j);
            sub(i, j);
            flag := flag and (p[j] >= e[j]);
        end;
        if flag then begin
            inc(c);
            a[c] := i + 1;
        end;
    end;

    writeln(c);
    for i := 1 to c do begin
        write(a[i], ' ');
    end;

    close(input);
    close(output);
end.

uses
    sysutils;

var
    n: longint;
    a, b, c, s: array [1..100] of longint;
    u: array [1..100] of boolean;
    count: longint;
    best: longint;

procedure sort;
var
    i, t, j, k, ops: longint;
begin
    for i := n downto 2 do begin
        if b[i] < b[i div 2] then writeln('botva');
    end;

    ops := 0;
    for i := n downto 1 do begin
        t := b[1]; b[1] := b[i]; b[i] := t;
        j := 1;
        while 2 * j <= i - 1 do begin
            k := j;
            if b[2 * j] < b[k] then k := 2 * j;
            if (2 * j + 1 <= i - 1) and (b[2 * j + 1] < b[k]) then k := 2 * j + 1;
            if j = k then break;
            inc(ops);
            t := b[j]; b[j] := b[k]; b[k] := t;
            j := k;
        end; 
    end;

    if ops > best then begin
        best := ops;
        c := a;
    end;

    inc(count);
end;


    
procedure rec(x: longint);
var
    i, l, r: longint;
begin
    if (x > n) then begin
        b := a;
        sort;
        exit;
    end;

    if x = 1 then
        l := 1
    else
        l := a[x div 2] + 1;

    r := n - (s[x] - 1);

    for i := l to r do begin
        if not u[i] then begin
            u[i] := true;
            a[x] := i;
            rec(x + 1);
            u[i] := false;
        end;
    end;
end;

var
    i: longint;

begin
    if paramcount = 0 then begin
        write('enter n: ');
        read(n);
    end else begin
        n := strtoint(paramstr(1));
    end;

    for i := n downto 1 do begin
        s[i] := 1;
        if 2 * i <= n then 
            s[i] := s[i] + s[2 * i];
        if 2 * i + 1 <= n then 
            s[i] := s[i] + s[2 * i + 1];
    end;

    rec(1);
    writeln(count);
    writeln(best);
    for i := 1 to n do begin
        write(c[i]);
    end;
    writeln;
end.
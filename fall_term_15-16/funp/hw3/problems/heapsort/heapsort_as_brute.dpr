uses
    sysutils;

var
    n: longint;
    a, b: array [1..100] of longint;
    count: longint;

procedure sort;
var
    i: longint;
begin
    for i := n downto 2 do begin
        if b[i] < b[i div 2] then exit;
    end;
    for i := 1 to n do begin
        write(b[i]);
    end;
    writeln;
    inc(count);
end;
    
procedure rec(x: longint);
var
    i, t: longint;
begin
    if x > n then begin
        b := a;
        sort;
        exit;
    end;
    a[x] := x;
    for i := 1 to x do begin
        t := a[i]; a[i] := a[x]; a[x] := t;
        rec(x + 1);
        t := a[i]; a[i] := a[x]; a[x] := t;
    end;
end;

begin
    if paramcount = 0 then begin
        write('enter n: ');
        read(n);
    end else begin
        n := strtoint(paramstr(1));
    end;
    rec(1);
    writeln(count);
end.
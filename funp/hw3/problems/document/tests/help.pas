unit help;

interface

procedure createdict(size: longint; maxl: longint);

procedure page(psize: longint);

procedure par(lines: longint);

procedure dump;

implementation

const
    MAX_LINE = 200;
    cons: array [1..20] of char = 'bcdfghjklmnpqrstvwxz';
    vowl: array [1..6] of char = 'aeiouy';
    seps: string = ' .-;!?@#$%^&*()';

var
    dict: array [1..20000] of string;     
    dsize: longint;
    ps: longint;
    line: array [1..20000] of string;
    n: longint;

function randstr(l: longint): string;
var
    i: longint;
    c: char;
    r: string;
begin
    r := '';
    for i := 1 to l do begin
        if i mod 2 = 0 then
            c := cons[random(20) + 1]
        else
            c := vowl[random(6) + 1];

        r := r + c;
    end;
    randstr := r;
end;

procedure createdict(size: longint; maxl: longint);
var
    i, j: longint;
    found: boolean;
begin
    dsize := size;
    for i := 1 to size do begin
        repeat
            dict[i] := randstr(1 + random(maxl));
            found := false;
            for j := 1 to i - 1 do begin
                if dict[i] = dict[j] then
                    found := true;
            end;
        until not found;
    end;
end;

function randline: string;
var
    i, j, ws: longint;
    r: string;
begin
    ws := random(20);
    r := '';
    for i := 1 to ws do begin
        r := r + dict[random(dsize) + 1];
        if i < ws then
        for j := 1 to random(5) + 1 do
            r := r + seps[random(length(seps)) + 1];
    end;

    if r = '' then  
        for j := 1 to random(20) + 1 do
            r := r + seps[random(length(seps)) + 1];

    for i := 1 to length(r) do
        if random(2) = 0 then
            r[i] := upcase(r[i]);

    // Trim if line is too long
    if length(r) > MAX_LINE then
        setlength(r, MAX_LINE);

    // Remove leading & trailing space
    while r[1] = ' ' do
        delete(r, 1, 1);
    while r[length(r)] = ' ' do
        setlength(r, length(r) - 1);

    randline := r;
end;

procedure page(psize: longint);
begin
    ps := psize;
end;

procedure par(lines: longint);
var
    i: longint;
begin
    if n > 0 then
    begin
        inc(n);
        line[n] := '';
    end;
    
    for i := 1 to lines do begin
        inc(n);
        line[n] := randline;
    end;
end;

procedure dump;
var
    i: longint;
begin
    writeln(ps);
    for i := 1 to n do begin
        writeln(line[i]);
    end;
end;

end.
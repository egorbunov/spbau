{$q+,r+,o-}

uses
    sysutils;

const
    max_l = 100;

type
    thist = array [1..26] of longint;

function hist(s: string): thist;
var
    i, j, t: longint;
begin
    fillchar(result, sizeof(result), 0);
    for i := 1 to length(s) do begin
        assert(('A' <= s[i]) and (s[i] <= 'Z'), format('Illegal symbol "%s"', [s[i]]));
        inc(result[ord(s[i]) - ord('A') + 1]);
    end;

    for i := 1 to 26 do begin
        for j := i + 1 to 26 do begin
            if result[i] < result[j] then begin
                t := result[i]; result[i] := result[j]; result[j] := t;
            end;
        end;
    end;
end;

var
    h1, h2: thist;
    s1, s2: string;
    i: longint;
    f: boolean;
begin
    assign(input, 'ancient.in'); reset(input);
    assign(output, 'ancient.out'); rewrite(output);

    readln(s1);
    assert((0 < length(s1)) and (length(s1) <= max_l), 'length(s1) is out of bounds');
    h1 := hist(s1);

    readln(s2);
    assert((0 < length(s2)) and (length(s2) <= max_l), 'length(s2) is out of bounds');
    h2 := hist(s2);

    f := true;
    for i := 1 to 26 do begin
        f := f and (h1[i] = h2[i]);
    end;

    if f then begin
        writeln('YES');
    end else begin
        writeln('NO');
    end;

    close(input);
    close(output);
end.
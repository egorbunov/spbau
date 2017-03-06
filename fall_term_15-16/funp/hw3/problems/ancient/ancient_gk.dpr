type
    thist = array [1..26] of longint;

function hist(s: string): thist;
var
    i, j, t: longint;
begin
    fillchar(result, sizeof(result), 0);
    for i := 1 to length(s) do begin
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
    h1 := hist(s1);

    readln(s2);
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
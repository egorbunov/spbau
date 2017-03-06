var
    i: longint;
    dict: array [1..1280] of string;

const
    cons: array [1..20] of char = 'bcdfghjklmnpqrstvwxz';
    vowl: array [1..6] of char = 'aeiouy';

function randstr(l: longint): string;
var
    i: longint;
    c: char;
    r: string;
begin
    r := '';
    for i := 1 to l do begin
        if random(2) = 0 then
            c := cons[random(20) + 1]
        else
            c := vowl[random(6) + 1];

        r := r + c;
    end;
    randstr := r;
end;

var
    f: longint;
    j: longint;
    found: boolean;

begin
    randseed := 120347;
    for i := 1 to 1024 do begin
        repeat
            if i <= 26 then
                dict[i] := randstr(1)
            else if i <= 702 then
                dict[i] := randstr(2)
            else
                dict[i] := randstr(3);
            found := false;
            for j := 1 to i - 1 do begin
                if dict[i] = dict[j] then
                    found := true;
            end;
        until not found;
    end;

    writeln(8);
    for i := 0 to 9 do begin
        f := 0;
        for j := 0 to 1023 do
            if (j and (1 shl i)) <> 0 then begin
                if f <> 0 then write(' ');
                inc(f);
                write(dict[j + 1]);
                if f mod 32 = 0 then begin
                    writeln;
                    f := 0;
                end;
            end;
        if i < 9 then writeln;
    end;
end.

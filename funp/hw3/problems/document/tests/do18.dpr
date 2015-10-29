var
    i: longint;
    dict: array [1..128] of string;

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
        if i mod 2 = 0 then
            c := cons[random(20) + 1]
        else
            c := vowl[random(6) + 1];

        r := r + c;
    end;
    randstr := r;
end;

var
    f: boolean;
    j: longint;
    found: boolean;

begin
    randseed := 120347;
    for i := 1 to 64 do begin
        repeat
            dict[i] := randstr(1 + random(3));
            found := false;
            for j := 1 to i - 1 do begin
                if dict[i] = dict[j] then
                    found := true;
            end;
        until not found;
    end;

    writeln(4);
    for i := 0 to 5 do begin
        f := false;
        for j := 0 to 63 do
            if (j and (1 shl i)) <> 0 then begin
                if f then write(' ');
                f := true;
                write(dict[j + 1]);
            end;
        writeln;
        writeln(dict[(1 shl i) + 1]);
        writeln(dict[(1 shl i) + 1]);
        writeln(dict[(1 shl i) + 1]);
        if i < 5 then writeln;
    end;
end.

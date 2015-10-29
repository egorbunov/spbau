var
    i: longint;
    prev: boolean;
begin
    writeln(4);
    randseed := 12390761;
    prev := true;
    for i := 1 to 6665 do begin
        if (random(2) = 0) or prev then begin
            writeln('abcdefghijklmnopqrstuvwxyz'[random(26)+1]);
            prev := false;
        end else begin
            writeln;
            prev := true;
        end;
    end;
end.
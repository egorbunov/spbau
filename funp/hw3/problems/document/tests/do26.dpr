var
    i: longint;
    prev: boolean;
begin
    writeln(100);
    randseed := 12390761;
    prev := true;
    for i := 1 to 6665 do begin
        if (random(2) = 0) or prev or (i = 6665) then begin
            writeln('a');
            prev := false;
        end else begin
            writeln;
            prev := true;
        end;
    end;
end.
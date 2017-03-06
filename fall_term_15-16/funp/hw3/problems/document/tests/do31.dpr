var
    i: longint;
    prev: boolean;
begin
    randseed := 340004;
    writeln(7);
    for i := 1 to 3999 do begin
        writeln('a');
        if i <> 3999 then
            writeln;
    end;
end.
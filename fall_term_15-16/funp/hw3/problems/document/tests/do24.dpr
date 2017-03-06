var
    i, j: longint;
begin
    writeln(4);
    for i := 1 to 196 do begin
        for j := 1 to i do begin
            write('a');
        end;
        writeln;
    end;
end.
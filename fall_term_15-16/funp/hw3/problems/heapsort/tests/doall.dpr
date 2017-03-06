var
    s, tn, v: string;
begin
    reset(input, 'tests.lst');
    while not seekeof do begin
        readln(s);
        tn := copy(s, 1, 2);
        delete(s, 1, 3);
        if pos(' ', s) <> 0 then
            v := copy(s, 1, pos(' ', s) - 1)
        else
            v := s;

        rewrite(output, tn);
        writeln(v);
        close(output);
    end;
    close(input);
end.
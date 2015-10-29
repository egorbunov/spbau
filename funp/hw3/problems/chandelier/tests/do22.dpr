{$apptype console}
uses
    tools, sysutils;

var
    i: longint;
begin
    randseed := 564321322;

    prog := 'a';
    for i := 2 to max_l - 3 do begin
        prog := prog + '1';
    end;

    output();
end.
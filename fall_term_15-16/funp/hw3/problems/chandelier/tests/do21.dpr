{$apptype console}
uses
    tools, sysutils;

var
    i: longint;
begin
    randseed := 564321321;

    prog := 'a';
    for i := 2 to max_l div 2 do begin
        prog := prog + 'a2';
    end;

    output();
end.
uses
    tools, sysutils;

const
    top = 5;

var
    i: longint;
begin
    randseed := 564321309;

    for i := 1 to top - 1 do begin
        prog := prog + ring(5);
    end;
    prog := prog + ring(4);

    prog := prog + chr(top + ord('0'));

    shuffle();
    output();
end.
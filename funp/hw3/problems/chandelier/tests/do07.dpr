uses
    tools, sysutils;

const
    top = 5;

var
    i: longint;
begin
    randseed := 564321307;

    for i := 1 to top do begin
        prog := prog + ring(i);
    end;
    prog := prog + chr(top + ord('0'));

    shuffle();
    output();
end.
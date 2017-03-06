{$apptype console}
uses
    tools, sysutils;

begin
    randseed := 564321312;

    prog := randomCh12(20);

    output();
end.
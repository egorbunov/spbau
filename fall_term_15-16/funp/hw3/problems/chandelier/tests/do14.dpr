{$apptype console}
uses
    tools, sysutils;

begin
    randseed := 564321314;

    prog := randomCh23(100);

    output();
end.
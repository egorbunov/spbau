{$apptype console}
uses
    tools, sysutils;

begin
    randseed := 564321313;

    prog := randomCh23(20);

    output();
end.
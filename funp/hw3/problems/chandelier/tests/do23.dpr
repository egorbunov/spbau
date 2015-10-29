{$apptype console}
uses
    tools, sysutils;

begin
    randseed := 564321323;

    prog := randomCh12(max_l - 13);

    output();
end.
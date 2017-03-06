{$apptype console}
uses
    tools, sysutils;

begin
    randseed := 564321324;

    prog := randomCh23(max_l - 7);

    output();
end.
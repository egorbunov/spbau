(*
    Checker for NEERC'2004 Problem Y: Yield
    (C) Roman Elizarov
*)

{$O+,Q+,R+}
{$APPTYPE CONSOLE}

uses
    TestLib, SysUtils;

var
    ja, pa: Integer;
begin
    ja := ans.readInteger();
    pa := ouf.readInteger();

    if not ouf.SeekEof then 
        Quit(_PE, 'Extra information in the output file');

    if ja <> pa then
        Quit(_WA, format('%d instead of %d', [pa, ja]))
    else
        Quit(_OK, format('%d', [ja]));
end.

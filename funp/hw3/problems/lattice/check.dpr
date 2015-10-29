(*
    Checker for NEERC'2004 Problem L: Lattice Animals
    (C) Maxim Babenko, Roman Elizarov
*)

program Check;
uses
  TestLib, SysUtils;

var
  ja, pa: Integer;
begin
  ja := ans.readInteger;
  pa := ouf.readInteger;
  if not ouf.SeekEof then 
    Quit(_pe, 'Extra information in the output file');
  if ja <> pa then 
      Quit(_wa, format('"%d" instead of "%d"', [pa, ja]));
  Quit(_ok, IntToStr(ja));
end.

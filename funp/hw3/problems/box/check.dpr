(*
    Checker for NEERC'2004 Problem B: Box
    (C) Georgy Korneev, Roman Elizarov
*)

program Check;
uses
  TestLib, SysUtils;

function Parse(var stm: InStream): String;
begin
  Result := trim(stm.readString());
  if (Result <> 'POSSIBLE') and (Result <> 'IMPOSSIBLE') then
    stm.Quit(_pe, Format('"POSSIBLE" or "IMPOSSIBLE" expected instead of "%s"', [Result]));
  if not stm.SeekEof then
    stm.Quit(_pe, 'Extra information in the output file');
end;

var
  ja, pa: String;
begin
  ja := Parse(ans);
  pa := Parse(ouf);
  if ja <> pa then 
      Quit(_wa, format('"%s" instead of "%s"', [pa, ja]));
  Quit(_ok, ja);
end.

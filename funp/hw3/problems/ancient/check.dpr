(*
    Checker for NEERC'2004 Problem A: Ancient Cipher
    (C) Georgy Korneev, Roman Elizarov
*)

program Check;
uses
  TestLib, SysUtils;

function Parse(var stm: InStream): String;
begin
  Result := trim(stm.readString());
  if (Result <> 'YES') and (Result <> 'NO') then
    stm.Quit(_pe, Format('"YES" or "NO" expected instead of "%s"', [Result]));
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

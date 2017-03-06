(*
    Checker for NEERC'2004 Problem D: Document Index
    (C) Maxim Babenko, Roman Elizarov
*)

{$apptype CONSOLE}
{$Q+,R+,O-}

uses
  testlib, SysUtils;
	
var
  i : integer;
  ps, js : string;
	
begin
  i := 0;
  while not ouf.seekeof or not ans.seekeof do begin
    if ouf.seekeof or ans.seekeof then quit (_wa, 'Line count mismatch after line '+inttostr (i));
    inc(i);
    ps := trim(ouf.readstring);
    js := trim(ans.readstring);
    if ps <> js then 
      quit(_wa, format('Mismatch on line %d: "%s" instead of "%s"', [i, ps, js]));
  end;
  quit(_ok, format('%d line(s)', [i]));
end.

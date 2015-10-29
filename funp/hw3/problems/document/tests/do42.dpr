//AL: max number of words
var i:integer;
begin
  randseed:=2391717;
  writeln (4);
  for i:=1 to 3998 do begin
    writeln (chr (65+random (26)));
    writeln;
  end;
  writeln (chr (65+random (26)));
end.
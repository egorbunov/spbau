//AL: max number of pages
var i:integer;
begin
  randseed:=23917;
  writeln (4);
  for i:=1 to 2499 do begin
    writeln (chr (65+random (26)));
    writeln (chr (65+random (26)));
    writeln;
  end;
  writeln (chr (65+random (26)));
end.
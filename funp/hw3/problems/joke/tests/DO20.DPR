uses dorand;
type integer=longint;
var i:integer;
begin
  regint (19);
  writeln (1000);
  for i:=1 to 1000 do writeln (lr (0, 1000), ' ', lr (0, 1000));
end.
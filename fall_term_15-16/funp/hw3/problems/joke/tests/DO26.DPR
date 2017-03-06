uses dorand, tools;
type integer=longint;
var a, b, i:integer;
begin
  regint (25);
  repeat
    a:=lr (0, 1000);
    b:=lr (0, 1000);
  until (a+b=999);
  writeln (1000);
  for i:=1 to 1000 do writeln (a, ' ', b);
end.
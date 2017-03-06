uses dorand;
type integer=longint;
var i, a, b:integer;
begin
  regint (21);
  writeln (1000);
  for i:=1 to 1000 do begin
    repeat
      a:=lr (0, 1000);
      b:=lr (0, 1000);
    until a+b<1000;
    writeln (a, ' ', b);
  end;
end.
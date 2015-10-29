uses tools;
var s: string;
begin
  randseed := 89345;
  s := randomstr(100);
  writeln(s);
  writeln(reorderstr(s));
end.
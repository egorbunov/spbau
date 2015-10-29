uses tools;
var s: string;
begin
  randseed := 24223;
  s := randomstr(95);
  writeln(s);
  writeln(reorderstr(s));
end.
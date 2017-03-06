uses tools;
var s: string;
begin
  randseed := 3424;
  s := randomstr(97);
  writeln(s);
  writeln(reorderstr(s));
end.
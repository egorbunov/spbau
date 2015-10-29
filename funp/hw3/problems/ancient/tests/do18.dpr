uses tools;
var s: string;
begin
  randseed := -68;
  s := randomstr(99);
  writeln(s);
  writeln(reorderstr(s));
end.
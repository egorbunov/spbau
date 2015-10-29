uses tools;
var s: string;
begin
  randseed := 912394568;
  s := randomstr(91);
  writeln(s);
  writeln(reorderstr(s));
end.
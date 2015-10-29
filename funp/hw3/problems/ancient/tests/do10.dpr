uses tools;
var s: string;
var c: char;
begin
  randseed := 23892;
  s := '';
  for c := 'A' to 'Z' do s := s + c;
  writeln(s);
  writeln(reorderstr(s));
end.
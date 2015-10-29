{$apptype CONSOLE}
var
	i : integer;
	
begin
	writeln(100);
	for i := 1 to 100 do
		writeln(1, ' ', 1, ' ', 999, ' ', 999, ' ', i * 10 - 1);
end.

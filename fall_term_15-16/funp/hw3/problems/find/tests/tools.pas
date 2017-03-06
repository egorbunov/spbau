unit tools;

interface

procedure gengrid(n, m : integer);
procedure genrand(n : integer; seed : integer);
procedure gencircle(n : integer; s : array of integer);
procedure genzigzag(n : integer);

implementation

procedure genrand(n : integer; seed : integer);
var
	i : integer;
	
begin
	randseed := seed;
	writeln(n);
	for i := 1 to n do begin
		writeln(random(100) + 1, ' ', random(100) + 1);
	end;
end;

procedure gencircle(n : integer; s : array of integer);
var
	i, j : integer;
	used : array[0..99] of boolean;
	
begin
	writeln(n);
	j := 0;
	fillchar(used, sizeof(used), false);
	for i := 1 to n do begin
		repeat
			if not used[j] then break;
			j := (j + 1) mod n;
		until false;
		used[j] := true;	
		writeln(trunc(50 + 50 * cos(j / n * 2 * pi)), ' ', trunc(50 + 50 * sin(j / n * 2 * pi)));
		j:= (j + s[(i - 1) mod length(s)]) mod n;
	end;
end;

procedure gengrid(n, m : integer);
var
	i : integer;

begin
	writeln(n + m);
	for i := 1 to n do begin
		if odd(i) then
			writeln(0, ' ', i + 10)
		else
			writeln(100, ' ', i + 10);	
	end;
	
	for i := 1 to m do begin
		if odd(i) then
			writeln(i + 10, ' ', 0)
		else
			writeln(i + 10, ' ', 100);	
	end;
end;

procedure genzigzag(n : integer);
var
	x, y : integer;
	i : integer;
	
begin
	writeln((n div 4) * 4 + 2);
	x := 0;
	y := 100;
	for i := 1 to n div 4 do begin
		writeln(x, ' ', y);
		inc(x, 3);
		dec(y, 100);
		
		writeln(x, ' ', y);
		dec(x);
		
		writeln(x, ' ', y);
		inc(x, 3);
		inc(y, 100);
		
		writeln(x, ' ', y);
		dec(x);
	end;
	
	writeln(x, ' ', y);
	inc(x, 3);
	dec(y, 100);

	writeln(x, ' ', y);
end;

end.

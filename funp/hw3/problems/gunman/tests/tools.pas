{$O-}
unit tools;

interface

procedure gentouchyes(x0 : integer; n : integer; w : integer);
procedure genrandyes(m : integer; x0 : integer; x1, y1, z1 : integer; sz : integer; rs : integer);

procedure genrandno_x(m : integer; x0 : integer; x1, y1, z1 : integer; sz : integer; rs : integer);
procedure genrandno_y(m : integer; x0 : integer; x1, y1, z1 : integer; sz : integer; rs : integer);

implementation

uses math, sysutils;

var
	n : integer = 0;
	ox1, oy1, ox2, oy2, oz : array[1..100] of integer;

procedure append(x1, y1, x2, y2, z : integer);
begin
	inc(n);
	ox1[n] := x1;
	oy1[n] := y1;
	ox2[n] := x2;
	oy2[n] := y2;
	oz[n] := z;
end;

procedure writeoutput;
var
	i : integer;
	
begin
	writeln(n);
	for i := 1 to n do
		writeln(ox1[i], ' ', oy1[i], ' ', ox2[i], ' ', oy2[i], ' ', oz[i]);
end;
			
procedure do_genrandyes(m : integer; x0 : integer; x1, y1, z1 : integer; sz : integer; rs : integer);
var
	x, y : integer;
	z : integer;
	i : integer;
	s : integer;
	
	xl, xr, yl, yt : integer;

begin
	randseed := rs;

	for i := 1 to m do begin
		z := (i - 1) * (1000 div m) + random(1000 div m) + 1;
		x := trunc(x0 + (x1 - x0) * z / z1);
		y := trunc(y1 * z / z1);

		if (x <= 2) or (x >= 998) or
		   (y <= 2) or (y >= 998) then
			continue;

		s := random(sz) + 1;
		xl := max(x - s div 2, 1);
		xr := min(xl + s, 999);
		yl := max(y - s div 2, 1);
		yt := min(yl + s, 999);

		append(xl, yl, xr, yt, z);
	end;
end;

procedure genrandyes(m : integer; x0 : integer; x1, y1, z1 : integer; sz : integer; rs : integer);
begin
	do_genrandyes(m, x0, x1, y1, z1, sz, rs);
    writeoutput;
end;

procedure genrandno_x(m : integer; x0 : integer; x1, y1, z1 : integer; sz : integer; rs : integer);
var
	i : integer;
	a, b : integer;

begin
    do_genrandyes(m, x0, x1, y1, z1, sz, rs);
    i := random(n) + 1;
    a := 1000 - ox1[i];
    b := 1000 - ox2[i];
    ox1[i] := b;
    ox2[i] := a;
    writeoutput;
end;

procedure genrandno_y(m : integer; x0 : integer; x1, y1, z1 : integer; sz : integer; rs : integer);
var
	i : integer;
	a, b : integer;

begin
    do_genrandyes(m, x0, x1, y1, z1, sz, rs);
    i := random(n) + 1;
    a := 1000 - oy1[i];
    b := 1000 - oy2[i];
    oy1[i] := b;
    oy2[i] := a;
    writeoutput;
end;

procedure gentouchyes(x0 : integer; n : integer; w : integer);
var
	i : integer;
	
begin
	writeln(n);
	for i := 1 to n do begin
		if random(2) = 0 then
			writeln(1 + random(100), ' ', 1 + random(w), ' ', x0, ' ', 999 - random(w), ' ', i * 10 - 1)
		else	
			writeln(x0, ' ', 1 + random(w), ' ', 999 - random(100), ' ', 999 - random(w), ' ', i * 10 - 1);
	end;
end;

end.

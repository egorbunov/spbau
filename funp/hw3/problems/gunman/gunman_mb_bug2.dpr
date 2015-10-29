{$R+,Q+,C+,O-}
{$apptype CONSOLE}

uses
	SysUtils, Math;

const
	IN_FILE = 'gunman.in';
	OUT_FILE = 'gunman.out';

	MAX_N = 100;
    MAX_C = 1000;

	EPSILON = 1e-10;
    INFINITY = 1e10;

type
	real = extended;

var
	n : integer;
    x, y : array[1..2,1..MAX_N] of integer;
    z : array[1..MAX_N] of integer;

    ok : boolean;
    xz_xs, xz_zs, xz_xe, xz_ze : integer;
    yz_ye, yz_ze : integer;

function sign(x : integer) : integer;
begin
	if x > 0 then
    	result := 1
    else if x < 0 then
    	result := -1
    else
    	result := 0;
end;

function IsSplitter(xs, ys, xe, ye : integer; x1, y1, x2, y2 : integer) : boolean;
begin
	result := sign((x1 - xs) * (ye - ys) - (y1 - ys) * (xe - xs)) *
              sign((x2 - xs) * (ye - ys) - (y2 - ys) * (xe - xs)) <= 0;
end;

function CheckYZ(ys, zs, ye, ze : integer) : boolean;
var
	i : integer;

begin
	result := true;
    for i := 1 to n do
    	if not IsSplitter(ys, zs, ye, ze, y[1,i], z[i], y[2,i], z[i]) then begin
        	result := false;
            break;
        end;
end;

function CheckXZ(xs, zs, xe, ze : integer) : boolean;
var
	i : integer;

begin
	result := true;
    for i := 1 to n do
    	if not IsSplitter(xs, zs, xe, ze, x[1,i], z[i], x[2,i], z[i]) then begin
        	result := false;
            break;
        end;
end;

procedure Solve;
var
	i, j : integer;
    u, v : integer;

begin
	if n = 1 then begin
    	ok := true;
        xz_xs := 0; xz_zs := 0; xz_xe := x[1,1]; xz_ze := z[1];
        yz_ye := y[1,1]; yz_ze := z[1];
        exit;
    end;

	ok := false;
	for i := 1 to n do for u := 1 to 2 do begin
    	if CheckYZ(0, 0, y[u,i], z[i]) then begin
        	yz_ye := y[u,i];
            yz_ze := z[i];
        	ok := true;
        end;
    end;

    if not ok then
    	exit;

    ok := false;
    
	for i := 1 to n do for u := 1 to 1 do
    	for j := i + 1 to n do for v := 1 to 1 do
            if CheckXZ(x[u,i], z[i], x[v,j], z[j]) then begin
                xz_xs := x[u,i];
                xz_zs := z[i];
                xz_xe := x[v,j];
                xz_ze := z[j];
                ok := true;
            end;
end;

procedure ReadInput;
var
	i : integer;

begin
	reset(input, IN_FILE);
    read(n);
    assert((n >= 1) and (n <= MAX_N));
    assert(eoln);
    readln;

    for i := 1 to n do begin
    	read(x[1,i]);
        assert((x[1,i] > 0) and (x[1,i] < MAX_C));
        assert(not eoln);

    	read(y[1,i]);
        assert((y[1,i] > 0) and (y[1,i] < MAX_C));
        assert(not eoln);

    	read(x[2,i]);
        assert((x[2,i] > 0) and (x[2,i] < MAX_C));
        assert(not eoln);

    	read(y[2,i]);
        assert((y[2,i] > 0) and (y[2,i] < MAX_C));
        assert(not eoln);

    	read(z[i]);
        assert((z[i] > 0) and (z[i] < MAX_C));
        assert(eoln);
        readln;

        assert(x[1,i] < x[2,i]);
        assert(y[1,i] < y[2,i]);
        assert((i = 1) or (z[i] > z[i-1]));
    end;
    assert(eof);

    close(input);
end;

procedure WriteOutput;
var
	i : integer;

    function GetX(z : integer) : real;
    begin
    	result := xz_xs + (z - xz_zs) / (xz_ze - xz_zs) * (xz_xe - xz_xs);
    end;

    function GetY(z : integer) : real;
    begin
    	result := z / yz_ze * yz_ye;
    end;

begin
	rewrite(output, OUT_FILE);
    if ok then begin
    	writeln('SOLUTION');
        writeln(GetX(0):0:6);
        for i := 1 to n do
        	writeln(GetX(z[i]):0:6, ' ', GetY(z[i]):0:6, ' ', z[i]:0);
    end else
    	writeln('UNSOLVABLE');
	close(output);
end;

begin
	ReadInput;
	Solve;
    WriteOutput;
end.

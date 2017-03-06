{$R+,Q+,C+,O+}
{$MINSTACKSIZE $001000000}
{$MAXSTACKSIZE $001000000}
{$apptype CONSOLE}

{.$define DUMP}

uses
	SysUtils, Math;

const
	IN_FILE = 'lattice.in';
	OUT_FILE = 'lattice.out';

	MAX_N = 10;
	MAX_M = 20000;

type
	TFigure = array[0..MAX_N+1] of word;
	
	TFigures = record
		m : integer;
		fig : array[1..MAX_M] of TFigure;
	end;
	
var
	n : integer;
	s : integer;
	xs, ys : integer;
	answer : integer;

procedure DumpFigure(const f : TFigure);
var
	i, j : integer;

begin
	for i := 0 to n - 1 do begin
		for j := 0 to n - 1 do
			if f[i] and (1 shl j) <> 0 then
				write('*')
			else
				write('.');
		writeln;
	end;
end;

function GetShift(x : word) : integer;
begin
	if x = 0 then begin
		result := n;
		exit;
	end;
	result := 0;
	while not odd(x) do begin
		inc(result);
		x := x shr 1;
	end;
end;

procedure NormalizeFigure(var f : TFigure);
var
	i, j : integer;
	minShift : integer;
	
begin
	minShift := maxint;
	for i := 0 to s do
		minShift := min(minShift, GetShift(f[i]));

	i := 0;
	while (i < n) and (f[i] = 0) do
		inc(i);
	for j := i to s - 1 do
		f[j - i] := f[j] shr minShift;
	for j := s - i to s - 1 do
		f[j] := 0;
end;

function CompareFigures(const f1, f2 : TFigure) : integer;
var
	i : integer;
	
begin
	for i := 0 to s - 1 do begin
		result := f1[i] - f2[i];
		if result <> 0 then
			exit;
	end;
	result := 0;
end;

procedure ReflectFigure(var f : TFigure);
var
	i : integer;
	t : word;

begin
	for i := 0 to (s - 1) div 2 do begin
		t := f[i];
		f[i] := f[s - i - 1];
		f[s - i - 1] := t;
	end;	
end;

procedure RotateFigure(var f : TFigure);
var
	t : TFigure;
	i, j : integer;

begin
	fillchar(t, sizeof(t), 0);
	for i := 0 to s - 1 do begin
		t[i] := 0;
		for j := 0 to s - 1 do
			if f[j] and (1 shl (s - 1 - i)) <> 0 then
				t[i] := t[i] or (1 shl j);
	end;
	f := t;
end;

procedure CanonizeFigure(var f : TFigure);
var
	i : integer;
	minf : TFigure;

begin
	NormalizeFigure(f);
	minf := f;
	for i := 2 to 8 do begin
		RotateFigure(f);
		if i = 5 then
			ReflectFigure(f);
        NormalizeFigure(f);
		if CompareFigures(minf, f) > 0 then
			minf := f;
	end;
	f := minf;
end;

procedure ExtendFigure(var f : TFigure);
var
	i : integer;
	
begin
    for i := s downto 1 do
    	f[i] := f[i-1] shl 1;
    f[0] := 0;
end;

function CheckFigureSize(const f : TFigure; xs, ys : integer) : boolean;
var
	i : integer;
    
begin
	result := false;
    for i := 0 to ys - 1 do
    	if f[i] >= 1 shl xs then
        	exit;
    result := f[ys] = 0;        
end;

function CheckFigureFitSize(const f : TFigure; xs, ys : integer) : boolean;
begin
	result := CheckFigureSize(f, xs, ys) or CheckFigureSize(f, ys, xs);
end;

procedure SweepFigures(var f : TFigures);
var
	r : TFigures;
	i : integer;
	p : array[1..MAX_M] of integer;

	procedure QSort(l, r : integer);
	var
		i, j : integer;
		x : TFigure;
		t : integer;

	begin
		i := l;
		j := r;
		x := f.fig[p[l + random(r - l)]];
		while i <= j do begin
			while CompareFigures(f.fig[p[i]], x) < 0 do
				inc(i);
			while CompareFigures(x, f.fig[p[j]]) < 0 do
				dec(j);
			if i <= j then begin
				t := p[i];
				p[i] := p[j];
				p[j] := t;
				inc(i);
				dec(j);
			end;		
		end;
		if l < j then
			QSort(l, j);
		if i < r then
			QSort(i, r);
	end;

begin
	for i := 1 to f.m do begin
		CanonizeFigure(f.fig[i]);
		p[i] := i;
	end;
	if f.m > 0 then
		QSort(1, f.m);

	r.m := 0;
	for i := 1 to f.m do
		if ((r.m = 0) or (CompareFigures(r.fig[r.m], f.fig[p[i]]) <> 0)) and
		   CheckFigureFitSize(f.fig[p[i]], xs, ys) then begin
			inc(r.m);
			r.fig[r.m] := f.fig[p[i]];
		end;

	f := r;
end;

procedure MakeStep(var f : TFigures);
var
	r : TFigures;
	af, bf : TFigure;
	i : integer;
	x, y : integer;

begin
	r.m := 0;
	for i := 1 to f.m do begin
		af := f.fig[i];
		ExtendFigure(af);
		for x := 0 to s - 1 do
			for y := 0 to s - 1 do
				if (af[y] and (1 shl x) = 0) and
				   (((x > 0)     and (af[y] and (1 shl (x - 1)) <> 0)) or
				    ((x < s - 1) and (af[y] and (1 shl (x + 1)) <> 0)) or
				    ((y > 0)     and (af[y - 1] and (1 shl x) <> 0)) or
				    ((y < s - 1) and (af[y + 1] and (1 shl x) <> 0))) then begin
				   	bf := af;
				   	bf[y] := bf[y] or (1 shl x);
				   	inc(r.m);
				   	r.fig[r.m] := bf;
				end;
	end;

	SweepFigures(r);
	f := r;
end;

procedure Solve;
var
	i, j : integer;
	f : TFigures;

begin
	fillchar(f, sizeof(f), 0);
	f.m := 1;
	f.fig[1][0] := 1;

	for i := 2 to n do begin
		s := i + 1;
		MakeStep(f);
{$ifdef DUMP}		
		writeln('n = ', i, ', total = ', f.m);
		for j := 1 to f.m do begin
			DumpFigure(f.fig[j]);
			writeln;
		end;
{$endif}		
	end;
	
	answer := f.m;
end;

procedure WriteOutput;
begin
	rewrite(output, OUT_FILE);
	writeln(answer);
	close(output);
end;

procedure ReadInput;
begin
	reset(input, IN_FILE);
    read(n);
    assert((n >= 1) and (n <= MAX_N));
    assert(not eoln);
    read(xs);
    assert((xs >= 1) and (xs <= n));
    assert(not eoln);
    read(ys);
    assert((ys >= 1) and (ys <= n));
    assert(eoln);
    readln;
    assert(eof);
    close(input);
end;

begin
	ReadInput;
	Solve;
    WriteOutput;
end.

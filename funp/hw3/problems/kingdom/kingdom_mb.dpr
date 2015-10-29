{$R+,Q+,C+}
{$apptype CONSOLE}

uses SysUtils;

const
	IN_FILE = 'kingdom.in';
	OUT_FILE = 'kingdom.out';

	MAX_N = 100;
	MAX_M = 1000;

type
	TColor = (_white, _gray, _black);
	
var
	n, m : integer;
	g : array[1..MAX_N,1..MAX_N] of integer;
	ex, ey : array[1..MAX_M * 2] of integer;
	a1, b1, a2, b2 : integer;
	
	d : array[1..MAX_N] of integer;
	adj : array[1..MAX_N,1..MAX_N] of integer;
	
	stacks : array[0..2,1..MAX_M * 2] of integer;
	top : array[0..2] of integer;

	col : array[1..MAX_M * 2] of TColor;
	dist : array[1..MAX_M * 2] of integer;
	from : array[1..MAX_M * 2] of integer;
	
procedure ReadInput;
var
	i : integer;
	x, y : integer;
		
begin
	reset(input, IN_FILE);

	read(n);
	assert((n >= 2) and (n <= MAX_N));
	assert(not eoln);
	
	read(m);
	assert((m >= 1) and (m <= MAX_M));
	assert(not eoln);
	
	read(a1);
	assert((a1 >= 1) and (a1 <= n));
	assert(not eoln);
	
	read(b1);
	assert((b1 >= 1) and (b1 <= n));
	assert(not eoln);
	
	read(a2);
	assert((a2 >= 1) and (a2 <= n));
	assert(not eoln);
	
	read(b2);
	assert((b2 >= 1) and (b2 <= n));
	assert(eoln);
	
	readln;
	
	fillchar(g, sizeof(g), 0);
	fillchar(d, sizeof(d), 0);
	for i := 1 to m do begin
		read(x);
		assert((x >= 1) and (x <= n));
		assert(not eoln);
		read(y);
		assert((y >= 1) and (y <= n));
		assert(eoln);
		readln;

		assert(x <> y);
		assert(g[x,y] = 0);
		g[x,y] := i;
		g[y,x] := i + m;

		ex[i] := x;
		ey[i] := y;
		ex[i+m] := y;
		ey[i+m] := x;

		inc(d[x]);
		adj[x,d[x]] := y;
		inc(d[y]);
		adj[y,d[y]] := x;
	end;
    assert(eof);
	close(input);
	
	assert(g[a1,b1] <> 0);
	assert(g[a2,b2] <> 0);
end;

function IsEmpty(i : integer) : boolean;
begin
	result := top[i] = 0;
end;

procedure Push(i, value : integer);
begin
	inc(top[i]);
	stacks[i,top[i]] := value;
end;

function Pop(i : integer) : integer;
begin
	result := stacks[i, top[i]];
	dec(top[i]);
end;

procedure Solve;
var
	s : integer;
	grayCount : integer;
	i, j, k : integer;
	
	procedure Go(i, j, len : integer);
	begin
		if j = 0 then
			exit;
		if (col[j] = _white) or ((col[j] = _gray) and (dist[j] > dist[i] + len)) then begin
			col[j] := _gray;
			dist[j] := dist[i] + len;
			from[j] := i;
			Push((s + len) mod 3, j);
            inc(grayCount);
		end;
	end;

begin
	fillchar(top, sizeof(top), 0);
	fillchar(col, sizeof(col), ord(_white));
	fillchar(from, sizeof(from), 0);
	col[g[a1,b1]] := _gray;
	dist[g[a1,b1]] := 0;
	Push(0, g[a1, b1]);
	grayCount := 1;

	s := 0;
	while grayCount <> 0 do begin
		while IsEmpty(s) do
			s := (s + 1) mod 3;
		i := Pop(s);
		dec(grayCount);
		if col[i] = _black then
			continue;
		assert(col[i] = _gray);
        col[i] := _black;

		{ single step 1 }
		for j := 1 to d[ex[i]] do
			Go(i, g[adj[ex[i],j],ey[i]], 1);
		{ single step 2 }
		for j := 1 to d[ey[i]] do
			Go(i, g[ex[i], adj[ey[i],j]], 1);

		{ double step }
		for j := 1 to d[ex[i]] do
			for k := 1 to d[ey[i]] do
				if (adj[ex[i],j] <> ey[i]) or (adj[ey[i],k] <> ex[i]) then
					Go(i, g[adj[ex[i],j], adj[ey[i],k]], 2);
	end;

	assert(col[g[a2,b2]] = _black);
end;

procedure WriteOutput;
var
	i : integer;
	hops : integer;
	ans : array[1..MAX_M] of integer;
	
begin
	rewrite(output, OUT_FILE);
	
	hops := 1;
	i := g[a2,b2];
	repeat	
		ans[hops] := i;
		i := from[i];
		if i = 0 then
			break;
		inc(hops);
	until false;

	writeln(dist[g[a2,b2]], ' ', hops);
	for i := hops downto 1 do
		writeln(ex[ans[i]], ' ', ey[ans[i]]);
		
	close(output);
end;

begin
	ReadInput;
	Solve;
    WriteOutput;
end.

{$apptype CONSOLE}
{$q+,r+,c+,o+}

uses
	SysUtils;

const
	IN_FILE = 'irrelevant.in';
	OUT_FILE = 'irrelevant.out';

    MAX_N = 100000;
    MAX_M = 1000000000;
    MAX_P = 20;

var
	n, m : integer;
	
	ansCount : integer;
	ans : array[1..MAX_N] of integer;
	
	np : integer;
	p, q : array[1..MAX_P] of integer;

    pow : array[0..MAX_N,1..MAX_P] of integer;

procedure Factorize;
var
	a, i : integer;
	
begin
	np := 0;
	a := m;
    i := 2;
	while i * i <= a do begin
		if i * i > a then
			break;
		if a mod i = 0 then begin
			inc(np);
			p[np] := i;
			q[np] := 0;
			while a mod i = 0 do begin
				inc(q[np]);
				a := a div i;
			end;
		end;
        inc(i);
	end;
	
	if a > 1 then begin
		inc(np);
		p[np] := a;
		q[np] := 1;
	end;
end;

procedure BuildPow;
var
	i, j : integer;
    a : integer;

begin
    for j := 1 to np do
    	for i := 0 to n - 1 do begin
        	a := i div p[j];
            pow[i,j] := a;
            if a > 0 then
				inc(pow[i,j], pow[a,j]);
	    end;
end;

procedure Solve;
var
	i, j : integer;

	function IsGood(j : integer) : boolean;
	var
		i : integer;

	begin
		result := false;
		for i := 1 to np do begin
			if pow[n - 1, i] - pow[j, i] - pow[n - j - 1, i] < q[i] then
				exit;
		end;
		result := true;
	end;

begin
	Factorize;
    BuildPow;

	ansCount := 0;
	for i := 0 to n - 1 do
		if IsGood(i) then begin
			inc(ansCount);
			ans[ansCount] := i + 1;
		end;
end;

procedure ReadInput;
begin
	reset(input, IN_FILE);

	read(n);
	assert(not eoln);
	read(m);
	assert(eoln);
	readln;

    assert((n >= 1) and (n <= MAX_N));
    assert((m >= 2) and (m <= MAX_M));

	close(input);
end;

procedure WriteData;
var
	i : integer;

begin
	rewrite(output, OUT_FILE);
	writeln(ansCount);
	for i := 1 to ansCount do
		write(ans[i], ' ');
	writeln;	
	close(output);
end;

begin
	ReadInput;
    Solve;
    WriteData;
end.

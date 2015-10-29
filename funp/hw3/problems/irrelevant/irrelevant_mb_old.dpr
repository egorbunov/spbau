{$apptype CONSOLE}
{$q+,r+,c+}

uses
	SysUtils;

const
	IN_FILE = 'irrelevant.in';
	OUT_FILE = 'irrelevant.out';

    MAX_N = 2000;
    MAX_M = 1000000000;


var
	n, m : integer;
	binomial : array[0..MAX_N,0..MAX_N] of integer;
    
procedure Solve;
var
	i, j : integer;
    
begin
    binomial[0,0] := 1;
    for i := 1 to n - 1 do
    	for j := 0 to i do begin
        	binomial[i,j] := 0;
            if j >= 1 then
            	inc(binomial[i,j], binomial[i-1,j-1]);
            if j <= i - 1 then
            	inc(binomial[i,j], binomial[i-1,j]);
            binomial[i,j] := binomial[i,j] mod m;
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
	i, count : integer;

begin
	rewrite(output, OUT_FILE);
    count := 0;
    for i := 0 to n - 1 do
    	if binomial[n-1,i] = 0 then
        	inc(count);
    writeln(count);
    for i := 0 to n - 1 do
    	if binomial[n-1,i] = 0 then
        	write(i + 1, ' ');
    writeln;
	close(output);
end;

begin
	ReadInput;
    Solve;
    WriteData;
end.

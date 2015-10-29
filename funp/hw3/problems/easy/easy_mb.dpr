{$apptype CONSOLE}
{$q+,r+,c+}

uses
	SysUtils;
    
const
	IN_FILE = 'easy.in';
	OUT_FILE = 'easy.out';
	
	MAX_N = 100;
	MAX_K = 10000;
	EPSILON = 1e-6;

type
	real = extended;
			
var
	n, m, k : integer;
	p : array[1..MAX_K] of real;
	
	i : integer;
	sn, sm : real;
	pn, pm : real;
	
	flag : boolean;
		
begin
	reset(input, IN_FILE);
	
	read(m);
	assert(not eoln);
	read(n);
	assert(not eoln);
	read(k);
	assert(eoln);
	readln;

	assert((m >= 1) and (m < n) and (n <= MAX_N));
	assert((n < k) and (k <= MAX_K));

	for i := 1 to k do begin
		read(p[i]);
		assert((p[i] > 0) and (p[i] < 100));
		assert(eoln);
		readln;
	end;

	assert(eof);

	close(input);

	rewrite(output, OUT_FILE);

	sn := 0;
	for i := n downto 1 do
		sn := sn + p[i];
	sm := 0;
	for i := n downto n - m + 1 do
		sm := sm + p[i];

	for i := n to k do begin
		pn := sn / n;
		pm := sm / m;

		assert(abs(pm - pn) > EPSILON);
		
		if (pn < pm) and ((i = n) or not flag) then
			writeln('BUY ON DAY ', i)
		else
		if (pn > pm) and ((i = n) or flag) then
			writeln('SELL ON DAY ', i);	

		flag := pn < pm;
			
		if i < k then begin
			sn := sn + p[i+1] - p[i-n+1];
			sm := sm + p[i+1] - p[i-m+1];
		end;
	end;	
	
	close(output);
end.

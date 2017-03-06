{$R+,Q+,C+,O-}
{$M 1024,16000000}
{$apptype CONSOLE}

uses
	Math;

const
	IN_FILE = 'chandelier.in';
	OUT_FILE = 'chandelier.out';

	MAX_L = 100000;
	INFINITY = maxint div 2;

var
	s : string;
	p : integer;
	bestDepth : integer;
	bestSeq : string;

procedure Solve(var p : integer; var bestDepth : integer; var bestSeq : string);
var
	ch : char;

	depths : array[0..8] of integer;
	seqs : array[0..8] of string;

	count, i : integer;
	curDepth : integer;

	j, bestShift : integer;

begin
	assert(p >= 1);
	assert(s[p] in ['a','0'..'9']);

	ch := s[p];
	if ch = 'a' then begin
		bestDepth := 1;
		bestSeq := ch;
		dec(p);
	end else begin
		dec(p);
		count := ord(ch) - ord('0');
		for i := 1 to count do
			Solve(p, depths[count - i], seqs[count - i]);

		bestDepth := INFINITY;
		for i := 0 to count - 1 do begin
			curDepth := 0;
			for j := 0 to count - 1 do
				curDepth := max(curDepth, j + depths[(i + j) mod count]);
			if curDepth < bestDepth then begin
				bestDepth := curDepth;
				bestShift := i;
			end;
		end;

		bestSeq := '';
		for i := 0 to count - 1 do
			bestSeq := bestSeq + seqs[(i + bestShift) mod count];
		bestSeq := bestSeq + ch;
	end;
end;
		
begin
	reset(input, IN_FILE);
	assert(not eoln);
	readln(s);
	assert((length(s) > 1) and (length(s) <= MAX_L));
	assert(eof);
	close(input);

	p := length(s);
	Solve(p, bestDepth, bestSeq);

	rewrite(output, OUT_FILE);
	writeln(bestDepth);
	writeln(bestSeq);
	close(output);		
end.

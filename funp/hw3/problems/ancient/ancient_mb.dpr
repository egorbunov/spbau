{$R+,Q+,C+}
{$apptype CONSOLE}

const
	IN_FILE = 'ancient.in';
	OUT_FILE = 'ancient.out';

	MAX_L = 100;

type
	TStat = array[char] of integer;
		
var
	a, b : string;
	sa, sb : TStat;
	c : char;
	failed : boolean;

function GetStat(s : string) : TStat;
var
	i : integer;
	u, v : char;
	t : integer;
	
begin
	assert((length(s) > 0) and (length(s) <= MAX_L));
	
	fillchar(result, sizeof(result), 0);
	for i := 1 to length(s) do begin
		assert(s[i] in ['A'..'Z']);
		inc(result[s[i]]);
	end;	

	for u := 'A' to 'Z' do
		for v := succ(u) to 'Z' do
			if result[u] > result[v] then begin
				t := result[u];
				result[u] := result[v];
				result[v] := t;
			end;
end;
		
begin
	reset(input, IN_FILE);
	assert(not eoln);
	readln(a);
	sa := GetStat(a);
	assert(not eoln);
	readln(b);
	sb := GetStat(b);
	assert(eof);
	close(input);
	
	failed := false;
	for c := #0 to #255 do
		if sa[c] <> sb[c] then
			failed := true;
			
	rewrite(output, OUT_FILE);
	if failed then
		writeln('NO')
	else
		writeln('YES');	
	close(output);		
end.

{$R+,Q+,C+}
{$apptype CONSOLE}

const
	IN_FILE = 'box.in';
	OUT_FILE = 'box.out';

	MIN_DIM = 1;
	MAX_DIM = 10000;

type
	TRect = record
		w, h : integer;
	end;
		
var
	r : array[1..6] of TRect;
	i : integer;
	
	t : array[1..6] of TRect;
	
	failed : boolean;
	used : array[1..6] of boolean;
	
	function AreEqual(const a, b : TRect) : boolean;
	begin
		AreEqual := (a.w = b.w) and (a.h = b.h);
	end;
	
procedure Check(i : integer);
var
	j : integer;
	
begin
	if i > 6 then begin
		if
			AreEqual(t[1], t[2]) and AreEqual(t[3], t[4]) and AreEqual(t[5], t[6]) and
			(t[2].w = t[5].w) and (t[2].h = t[3].w) and (t[5].h = t[3].h)
		then
			failed := false;
		exit;
	end;
	
	for j := 1 to 6 do
		if not used[j] then begin
			used[j] := true;
			t[i].w := r[j].w; t[i].h := r[j].h;
			Check(i + 1);
			t[i].w := r[j].h; t[i].h := r[j].w;
			Check(i + 1);
			used[j] := false;	
		end;
end;

		
begin
	reset(input, IN_FILE);
	for i := 1 to 6 do begin
		assert(not eoln);
		read(r[i].w);
		assert((r[i].w >= MIN_DIM) or (r[i].w <= MAX_DIM));
		read(r[i].h);
		assert((r[i].h >= MIN_DIM) or (r[i].h <= MAX_DIM));
		assert(eoln);
		readln;
	end;
	assert(eof);
	close(input);

	failed := true;
	fillchar(used, sizeof(used), false);
	Check(1);
			
	rewrite(output, OUT_FILE);
	if failed then
		writeln('IMPOSSIBLE')
	else
		writeln('POSSIBLE');	
	close(output);		
end.

{$apptype CONSOLE}
{$q+,r+,c+}

const
	IN_FILE = 'heapsort.in';
	OUT_FILE = 'heapsort.out';
	
	MAX_N = 50000;
	
var
	a : array[1..MAX_N] of integer;
	n : integer;	
	
	i, j, k : integer;
	tmp : integer;
	
begin
	reset(input, IN_FILE);
	read(n);
	assert((n >= 1) and (n <= MAX_N));
	assert(eoln);
	readln;
	assert(eof);
	close(input);
	
	a[1] := 1;
	for i := 2 to n do begin
		a[i] := a[i-1];
		a[i-1] := i;
		j := i - 1;
		while j <> 1 do begin
			k := j div 2;
			if a[k] < a[j] then begin
				tmp := a[k];
				a[k] := a[j];
				a[j] := tmp;
			end;	
			j := k;
		end;		
	end;
	
	rewrite(output, OUT_FILE);
	for i := 1 to n do
		write(a[i], ' ');
	writeln;	
	close(output);
end.

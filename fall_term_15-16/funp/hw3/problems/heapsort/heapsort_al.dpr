{$O-,Q+,R+}
const MaxN=50000;

var a:array [1..50000] of integer;
    i, x, n:integer;

begin
  assign (input, 'heapsort.in'); reset (input);
  assign (output, 'heapsort.out'); rewrite (output);
  read (n); assert ((n>=1) and (n<=MaxN) and seekeof);
  for i:=1 to n do begin
    x:=i-1; while x>1 do begin a[x]:=a[x shr 1]; x:=x shr 1 end;
    a[1]:=i; a[i]:=1;
  end;
  for i:=1 to n do write (a[i], ' ');
end.
{$o-,q+,r+}
uses Math, SysUtils;

const MaxN=1000;
      MaxAB=1000;

var a, b, o, q, c, v, w:array [1..MaxN] of integer;


function less (x, y:integer):boolean;
begin
  less:=(b[x]<b[y]) or ((b[x]=b[y]) and (a[x]<a[y]));
end;

procedure qsort (l, r:integer);
var i, j, x, y:integer;
begin
  if l>=r then exit;
  i:=l; j:=r; x:=o[(l+r) shr 1];
  repeat
    while less (o[i], x) do inc (i);
    while less (x, o[j]) do dec (j);
    if i<=j then begin
      y:=o[i]; o[i]:=o[j]; o[j]:=y;
      inc (i); dec (j)
    end
  until i>j;
  qsort (l, j);
  qsort (i, r);
end;


var n, i, j, p, p0, oc:integer;
    f, r:array [0..MaxN] of integer;
    g:array [1..MaxN] of boolean;

begin
  assign (input, 'joke.in'); reset (input);
   assign (output, 'joke.out'); rewrite (output);
  assert (not seekeoln); read (n); assert ((n>=1) and (n<=MaxN));
  assert (seekeoln); readln;
  for i:=1 to n do begin
    assert (not seekeoln); read (a[i]); assert ((a[i]>=0) and (a[i]<=MaxAB));
    assert (not seekeoln); read (b[i]); assert ((b[i]>=0) and (b[i]<=MaxAB));
    b[i]:=n-b[i];
    assert (seekeoln); readln;
  end;
  assert (seekeof);
  oc:=0;
  for i:=1 to n do if a[i]<b[i] then begin inc (oc); o[oc]:=i end;
  if oc=0 then begin
    write (n); for j:=1 to n do write (' ', j); writeln; halt
  end;
  qsort (1, oc);
  p:=1; j:=1; q[1]:=1; c[1]:=1; v[1]:=o[1]; w[1]:=1;
  for i:=2 to oc do begin
    if (a[o[i]]=a[o[i-1]]) and (b[o[i]]=b[o[i-1]]) then inc (c[p]) else begin
      if c[p]>b[o[i-1]]-a[o[i-1]] then c[p]:=b[o[i-1]]-a[o[i-1]];
      inc (p); c[p]:=1; j:=i; v[p]:=o[i]; w[p]:=i;
    end;
    q[i]:=j
  end;
  if c[p]>b[o[oc]]-a[o[oc]] then c[p]:=b[o[oc]]-a[o[oc]];
  p0:=1;
  for i:=1 to n do begin
    f[i]:=f[i-1]; r[i]:=0;
    while (p0<=p) and (b[v[p0]]<=i) do begin
      assert (a[v[p0]]<i);
      if f[a[v[p0]]]+c[p0]>f[i] then begin
        f[i]:=f[a[v[p0]]]+c[p0];
        r[i]:=p0;
      end;
      inc (p0);
    end;
  end;
  write (n-f[n]);
  j:=n; while j>0 do begin
    if r[j]=0 then dec (j) else begin
      for i:=w[r[j]] to w[r[j]]+c[r[j]]-1 do g[o[i]]:=true;
      j:=a[v[r[j]]];
    end;
  end;
  for i:=1 to n do if not g[i] then write (' ', i); writeln;
end.
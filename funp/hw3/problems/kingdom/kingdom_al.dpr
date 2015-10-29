{$o-,q+,r+}
uses SysUtils;

const MaxN=100;
      MaxM=1000;

var g:array [1..MaxN, 1..MaxN] of boolean;
    h:array [0..2*MaxM-1, 0..2*MaxM-1] of integer;
    x, y:array [0..2*MaxM-1] of integer;
    d, p, l:array [0..2*MaxM-1] of integer;
    f:array [0..2*MaxM-1] of boolean;
    min, mi, s, e, n, m, a1, a2, b1, b2, i, j:integer;

begin
  assign (input, 'kingdom.in'); reset (input);
  assign (output, 'kingdom.out'); rewrite (output);
  assert (not seekeoln); read (n); assert ((n>=2) and (n<=MaxN));
  assert (not seekeoln); read (m); assert ((n>=1) and (n<=MaxM));
  assert (not seekeoln); read (a1); assert ((a1>=1) and (a1<=n));
  assert (not seekeoln); read (b1); assert ((b1>=1) and (b1<=n) and (b1<>a1));
  assert (not seekeoln); read (a2); assert ((a2>=1) and (a2<=n));
  assert (not seekeoln); read (b2); assert ((b2>=1) and (b2<=n) and (b2<>a2));
  assert ((a1<>b1) or (a2<>b2));
  assert (seekeoln); readln; s:=-1; e:=-1;
  for i:=0 to m-1 do begin
    assert (not seekeoln); read (x[i]); assert ((1<=x[i]) and (x[i]<=n));
    assert (not seekeoln); read (y[i]); assert ((1<=y[i]) and (y[i]<=n) and (x[i]<>y[i]));
    assert (not g[x[i], y[i]]);
    g[x[i], y[i]]:=true; 
    g[y[i], x[i]]:=true;
    if (x[i]=a1) and (y[i]=b1) then s:=i else
    if (x[i]=b1) and (y[i]=a1) then s:=i+m;
    if (x[i]=a2) and (y[i]=b2) then e:=i else
    if (x[i]=b2) and (y[i]=a2) then e:=i+m;
    x[i+m]:=y[i]; y[i+m]:=x[i];
    assert (seekeoln); readln;
  end;
  assert (g[a1, b1] and g[a2, b2]);
  assert (seekeof);
  fillchar (h, sizeof (h), 63);
  for i:=0 to 2*m-1 do
    for j:=0 to 2*m-1 do 
      if i mod m<>j mod m then begin
        if (x[i]=x[j]) and g[y[i], y[j]] then h[i, j]:=1;
        if (y[i]=y[j]) and g[x[i], x[j]] then h[i, j]:=1;
        if g[x[i], x[j]] and g[y[i], y[j]] then h[i, j]:=2;
      end;
  fillchar (d, sizeof (d), 63);
  d[e]:=0; l[e]:=0;
  repeat
    min:=maxint; mi:=-1;
    for i:=0 to 2*m-1 do 
      if not f[i] and (d[i]<min) then begin min:=d[i]; mi:=i end;
    assert (min<1000000);
    f[mi]:=true;
    for i:=0 to 2*m-1 do
      if not f[i] and (d[mi]+h[mi, i]<d[i]) then begin
        d[i]:=d[mi]+h[mi, i]; p[i]:=mi; l[i]:=l[mi]+1;
      end;
  until f[s];
  writeln (d[s], ' ', l[s]+1);
  writeln (x[s], ' ', y[s]);
  repeat
    s:=p[s];
    writeln (x[s], ' ', y[s]);
  until s=e;
end.
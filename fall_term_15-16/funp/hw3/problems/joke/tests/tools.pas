unit tools;
interface
uses dorand;
type integer=longint;
const MaxN=1000;
procedure Init (_n:integer);
procedure GenPosPoisson (E:extended);
procedure GenPosDistr (func:rands);
procedure Create;
procedure SmallLie (lp, lq:integer);
procedure HugeLie (lp, lq:integer);{PolyGon}
procedure Save;
procedure Merge (p1, p2:integer);
procedure Display;
implementation

var p, psav:array [1..MaxN] of integer;
    a, b, asav, bsav:array [1..MaxN] of integer;
    pc, N:integer;


procedure Init (_n:integer);
begin
  regint (_n); N:=_n;
end;

procedure GenPosPoisson (E:extended);
var l:integer;
begin
  l:=N; 
  repeat
    inc (pc); p[pc]:=Poisson (E, 1, l);
    dec (l, p[pc]);
  until l=0;
end;


procedure GenPosDistr (func:rands);
var l:integer;
begin
  l:=N;
  repeat
    inc (pc); p[pc]:=longrandd (1, l, func);
    dec (l, p[pc]);       
  until l=0;
end;


procedure Create;
var i, j, x, nc:integer;
begin
  nc:=0;
  for i:=1 to pc do begin
    x:=nc;
    for j:=1 to p[i] do begin
      inc (nc); a[nc]:=x; b[nc]:=n-p[i]-x;
    end;
  end;
end;


procedure SmallLie (lp, lq:integer);
var i:integer;
begin
  for i:=1 to n do
    if random (lq)<lp then begin
      if coin then inc (a[i], random (2)*2-1)
              else inc (b[i], random (2)*2-1);
      if a[i]<0 then inc (a[i], 2);
      if b[i]<0 then inc (b[i], 2);
    end;
end;


procedure HugeLie (lp, lq:integer);{PolyGon}
var i:integer;
begin
  for i:=1 to n do
    if random (lq)<lp then begin
      repeat
        a[i]:=lr (0, N-1);
        b[i]:=lr (0, N-1);
      until a[i]+b[i]<=N-1;
    end;
end;


procedure Save;
begin
  psav:=p; asav:=a; bsav:=b; pc:=0;
end;


procedure Merge (p1, p2:integer);
var i:integer;
begin
  for i:=1 to N do
    if random (p1+p2)<p1 then begin a[i]:=asav[i]; b[i]:=bsav[i] end;
end;


procedure Display;
var o:array [1..MaxN] of integer;
    i, t:integer;
begin
  for i:=1 to N do begin t:=random (i)+1; o[i]:=o[t]; o[t]:=i end;
  writeln (N);
  for i:=1 to N do writeln (a[o[i]], ' ', b[o[i]]);
end;

end.
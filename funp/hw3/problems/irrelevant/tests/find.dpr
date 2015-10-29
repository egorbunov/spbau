//finding tool
{$O-,Q+,R+}
uses SysUtils, dorand;

type integer=longint;

const MaxN=100000;
      MaxM=1000000000;


function isat (n, m, p, k:integer):boolean;
var cy:integer;
begin
  dec (n, m); 
  cy:=0;
//  writeln (n, ' ', m, ' ', p, ' ', k);
  while (m>0) or (n>0) do begin
    if m mod p + n mod p + cy >= p then begin 
      dec (k);
      cy:=1;
      if k=0 then begin Result:=true; exit end;
    end else cy:=0;
    n:=n div p; m:=m div p
  end;
  Result:=false;
end;

var n:integer;
    t:array [1..MaxN+1] of integer;
    tc:integer;

procedure integrate (p, k:integer);
var i, pc:integer;
begin
  pc:=0;
  for i:=1 to tc do 
    if isat (n, t[i], p, k) then begin inc (pc); t[pc]:=t[i] end;
  tc:=pc;
end;


var i, m, mm, k, maxtc, mn:integer;


begin
  randomize;
  maxtc:=maxint; mn:=-1;
  n:=99999;
  m:=100000;
  repeat
    (*n:=lr (1900, 1999);
    m:=lr (100000000, 1000000000);*) mm:=m;
    for i:=0 to n do t[i+1]:=i;
    tc:=n+1;
    for i:=2 to m do begin
      if i*i>m then break;
      k:=0; while m mod i = 0 do begin inc (k); m:=m div i end;
      if k>0 then integrate (i, k);
    end;
    if m>1 then integrate (m, 1);
    if (tc>0) and (tc<maxtc) then begin mn:=mm; maxtc:=tc; writeln (n+1, ' ', mm, ' ', maxtc); end;
    if (tc=2) then writeln (n+1, ' ', mm, ' ', tc);
    //if (tc=2) and (t[2]-t[1]>400) and (t[2]-t[1]<600) then writeln (n, ' ', mm, ' ', tc);
    //if (tc=3) and (t[3]-t[1]>1400) and (t[3]-t[1]<1600) then begin writeln (n+1, ' ', mm, ' ', tc); halt end;
    dec (mm); m:=mm;
  until m=1;
  writeln (mn, ' ', maxtc);
end.
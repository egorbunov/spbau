//solution perfectly works for N up to 200000
//complexity is about N*log(N)*log(M)
{$O-,Q+,R+}
uses SysUtils;

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


var i, m, k:integer;


begin
  assign (input, 'irrelevant.in'); reset (input);
  assign (output, 'irrelevant.out'); rewrite (output);
  read (n, m); assert (seekeof);
  assert ((n>=1) and (n<=MaxN) and (m>=2) and (m<=MaxM));
  dec (n);
  for i:=0 to n do t[i+1]:=i;
  tc:=n+1;
  for i:=2 to m do begin
    if i*i>m then break;
    k:=0; while m mod i = 0 do begin inc (k); m:=m div i end;
    if k>0 then integrate (i, k);
  end;
  if m>1 then integrate (m, 1);
  writeln (tc);
  for i:=1 to tc do write (t[i]+1, ' ');
end.
(*
    Checker for NEERC'2004 Problem J: Joke with Turtles
    (C) Andrew Lopatin
*)

{$O-,Q+,R+}
{$APPTYPE CONSOLE}
uses testlib, SysUtils;
type integer=longint;

const MaxN=1000;

var i, j, cur, n, jury, cont:integer;
    f:array [1..MaxN] of boolean;
    a, b, c, d:array [1..MaxN] of integer;
    l:array [1..MaxN] of integer;

begin
  n:=inf.readlongint;
  jury:=ans.readlongint;
  cont:=ouf.readlongint;
  
  if cont>jury then begin
    for j:=1 to cont do ouf.readlongint; //expect PE
    if not ouf.seekeof then quit (_PE, 'too many numbers in output file');
    quit (_WA, 'answer too bad: '+inttostr (cont)+' instead of '+inttostr (jury));
  end;
  
  if cont<0 then 
    quit (_PE, 'number of lying turtles cannot be negative: '+inttostr (cont));
  //otherwise it may lead to PE after checking eof.
  
  for j:=1 to cont do l[j]:=ouf.readlongint;
  
  if not ouf.seekeof then quit (_PE, 'too many numbers in output file');
  
  for j:=1 to cont do begin
    cur:=l[j];
    if (cur<1) or (cur>n) then quit (_WA, 'invalid number of turtle '+inttostr (cur));
    if f[cur] then quit (_WA, 'turtle '+inttostr (cur)+' already lied');
    f[cur]:=true;
  end;
  
  for i:=1 to n do begin a[i]:=inf.readlongint; b[i]:=inf.readlongint end;
  
  for i:=1 to n do 
    if not f[i] then begin
      if a[i]+b[i]>=n then quit (_WA, 'turtle '+inttostr (i)+' is obviously lying');
      for j:=a[i]+1 to n-b[i] do begin
        if (d[j]<>0) and ((a[i]<>a[d[j]]) or (b[i]<>b[d[j]])) then 
          quit (_WA, 'intersection of intervals for turtles '+
                  inttostr (i)+' and '+inttostr (d[j]));
        d[j]:=i;
        inc (c[j]);
        if c[j]>n-a[i]-b[i] then 
          quit (_WA, 'too many instances of the segment for turtle '+inttostr (i));
      end;
    end;
  
  if cont<jury then quit (_Fail, 'Found better answer: '+inttostr (cont)+' instead of '+inttostr (jury));
  
  quit (_OK, inttostr (n)+' turtles, '+inttostr (cont)+' are lying');
end.
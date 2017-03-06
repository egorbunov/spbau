unit tools2;
interface
uses dorand;
type integer=longint;
procedure gend0 (func:rands);
procedure gend1 (func:rands);
procedure gend0f (func:rands);
procedure gend1f (func:rands);
procedure genstring;
procedure smallchange;
implementation

var l:array [1..26] of integer;

procedure gend0 (func:rands);
var i, leftl:integer;
begin
  leftl:=100;
  for i:=1 to 26 do begin
    l[i]:=longrandd (0, max (round (2*leftl/(26-i+1)), 0), func);
    if l[i]>leftl then l[i]:=leftl;
    dec (leftl, l[i]);
  end;
end;

procedure gend1 (func:rands);
var i, leftl:integer;
begin
  leftl:=100;
  for i:=1 to 26 do begin
    l[i]:=longrandd (1, max (round (2*leftl/(26-i+1)-1), 1), func);
    if l[i]>leftl then l[i]:=leftl;
    dec (leftl, l[i]);
  end;
end;


procedure gend1f (func:rands);
var i, leftl, cur:integer;
begin
  leftl:=100;
  for i:=1 to 26 do begin
    cur:=longrandd (1, max (round (2*leftl/(26-i+1)-1), 1), func);
    if cur>leftl then cur:=leftl;
    dec (leftl, cur);
    inc (l[i], cur);
  end;
  repeat
    for i:=1 to 26 do begin
      cur:=longrandd (0, max (round (2*leftl/(26-i+1)-1), 0), func);
      if cur>leftl then cur:=leftl;
      dec (leftl, cur);
      inc (l[i], cur);
    end;
  until leftl=0;
end;


procedure gend0f (func:rands);
var i, leftl, cur:integer;
begin
  leftl:=100;
  for i:=1 to 26 do begin
    cur:=longrandd (0, max (round (2*leftl/(26-i+1)-1), 0), func);
    if cur>leftl then cur:=leftl;
    dec (leftl, cur);
    inc (l[i], cur);
  end;
  repeat
    for i:=1 to 26 do begin
      cur:=longrandd (0, max (round (2*leftl/(26-i+1)-1), 0), func);
      if cur>leftl then cur:=leftl;
      dec (leftl, cur);
      inc (l[i], cur);
    end;
  until leftl=0;
end;


procedure genstring;
var o1:array [1..26] of integer;
    o2:array [1..100] of integer;
    s:string;
    i, j, t:integer;
begin
  for i:=1 to 26 do begin t:=random (i)+1; o1[i]:=o1[t]; o1[t]:=i end;
  s:='';
  for i:=1 to 26 do
    for j:=1 to l[o1[i]] do
      s:=s+chr (64+i);
  for i:=1 to length (s) do begin t:=random (i)+1; o2[i]:=o2[t]; o2[t]:=i end;
  for i:=1 to length (s) do write (s[o2[i]]);
  writeln;
end;


procedure smallchange;
var x, y:integer;
begin
  repeat
    x:=lr (1, 26); y:=lr (1, 26);
  until (x<>y) and (l[x]>0);
  dec (l[x]); inc (l[y]);
end;

end.
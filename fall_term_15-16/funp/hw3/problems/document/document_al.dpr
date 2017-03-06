{$O-,Q+,R+}
uses SysUtils;

const MaxFSize=20000;
      MinN=4;
      MaxN=100;
      MaxL=200;
      MaxWords=MaxFSize shr 1; // actually less
      MaxLines=MaxFSize shr 1;
      MaxPages=MaxFSize shr 3 + 2; //I've seen no test reaching this :(

var lno:integer;
    s:string;
    c:char;

procedure nextline;
begin
  s:=''; 
  while c<>#13 do begin
    assert (c in [#32..#126]);
    s:=s+upcase (c);
    read (c);
  end;
  read (c); assert (c=#10);
  read (c);
  inc (lno);
  if length (s)<>0 then begin
    assert ((s[1]<>' ') and (s[length (s)]<>' '));
  end;
end;


var p:integer;

function nextword:string;
var op:integer;
begin
  while not (s[p] in [#0,'A'..'Z']) do inc (p);
  op:=p; while s[p] in [#0,'A'..'Z'] do inc (p);
  Result:=copy (s, op, p-op);
  while not (s[p] in [#0,'A'..'Z']) do inc (p);
end;

var w:array [1..MaxWords] of string;
    wl, wi:array [1..MaxWords] of integer;


procedure qsort (l, r:integer);
var i, j, x, y:integer;
begin
  if l>=r then exit;
  i:=l; j:=r; x:=wi[(l+r) shr 1];
  repeat
    while w[wi[i]]<w[x] do inc (i);
    while w[x]<w[wi[j]] do dec (j);
    if i<=j then begin
      y:=wi[i]; wi[i]:=wi[j]; wi[j]:=y;
      inc (i); dec (j);
    end;
  until i>j;
  qsort (l, j);
  qsort (i, r);
end;


var lm:array [1..MaxLines] of integer;
    lblank, pc, ofs, n:integer;

procedure doblank;
var len, ptr:integer;
begin
  len:=lno-lblank-1; ptr:=lblank+1;
  if ((len in [2..3]) and (ofs+len=n+1)) or
     ((len>1) and (ofs=n-1)) then begin
    inc (pc); ofs:=0;
  end;
  repeat
    lm[ptr]:=pc;
    inc (ptr); inc (ofs); dec (len);
    if (ofs=n) or ((len=2) and (ofs=n-1)) then begin inc (pc); ofs:=0 end;
  until len=0;
  if ofs>0 then inc (ofs);
  if ofs=n then begin inc (pc); ofs:=0 end;
end;

var z:array [1..MaxPages] of boolean;


procedure doz (w:string);
var i, beg:integer;
    state, was:boolean;
begin
  state:=false; was:=false; beg:=-1;
  write (w, ' ');
  for i:=1 to MaxPages do begin
    if z[i] and not state then begin
      state:=true; beg:=i
    end;
    if not z[i] and state then begin
      if was then write (',');
      was:=true;
      if i-beg=1 then write (beg) else
      if i-beg=2 then write (beg, ',', beg+1) else
      write (beg, '-', i-1);
      state:=false;
    end;
  end;
  writeln;
end;

var f:file of byte;
    i, tmp, wc:integer;
    t:string;


begin
  assign (f, 'document.in'); reset (f);
  assert (filesize (f)<=MaxFSize);
  close (f);
  assign (input, 'document.in'); reset (input);
  assign (output, 'document.out'); rewrite (output);
  lno:=-1; read (c); nextline;
  for i:=1 to length (s) do assert (s[i] in ['0'..'9']);
  assert (length (s)<=3);
  val (s, n, tmp); assert (tmp=0);
  assert ((n>=MinN) and (n<=MaxN));
  lblank:=0; wc:=0; ofs:=0; pc:=1;
  repeat
    nextline; assert (length (s)<=MaxL);
    if length (s)>0 then begin
      p:=1; s:=s+' '#0' '#0;
      t:=nextword; //assert (t<>#0);
      while t<>#0 do begin
        inc (wc); w[wc]:=t; wl[wc]:=lno;
        t:=nextword;
      end;
    end else begin
      assert (lno-lblank>1);
      doblank;
      lblank:=lno;
    end;
  until c=#26;
  inc (lno);//as empty line read
  assert (lno-lblank>1); // BUG!!! IT IS NOT SPECIFIED IN STATEMENT!!!
  doblank;
  for i:=1 to wc do wi[i]:=i;
  qsort (1, wc);
  p:=2;
  z[lm[wl[wi[1]]]]:=true;
  repeat
    if w[wi[p]]<>w[wi[p-1]] then begin
      doz (w[wi[p-1]]); fillchar (z, sizeof (z), 0);
    end;
    z[lm[wl[wi[p]]]]:=true;
    inc (p);
  until p>wc;
  doz (w[wi[wc]]);
end.
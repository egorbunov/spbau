{$O-,Q+,R+}
const MaxWH=10000;
      Sides=6;

var f:array [1..MaxWH] of integer;
    g:array [1..Sides*2, 1..Sides*2] of integer;
    i, cnt, w, h:integer;

begin
  assign (input, 'box.in'); reset (input);
  assign (output, 'box.out'); rewrite (output);
  cnt:=0;
  for i:=1 to 6 do begin
    assert (not seekeoln); read (w);
    assert (not seekeoln); read (h);
    assert (seekeoln); readln;
    assert ((w>=1) and (w<=MaxWH) and (h>=1) and (h<=MaxWH));
    if f[w]=0 then begin inc (cnt); f[w]:=cnt; end;
    if f[h]=0 then begin inc (cnt); f[h]:=cnt; end;
    inc (g[f[w], f[h]]); inc (g[f[h], f[w]]);
  end;
  case cnt of
    1:writeln ('POSSIBLE');
    2:if g[1, 2]<>4 then writeln ('IMPOSSIBLE') else writeln ('POSSIBLE');
    3:if (g[1, 2]<>2) or (g[1, 3]<>2) or (g[2, 3]<>2) then 
           writeln ('IMPOSSIBLE')
      else writeln ('POSSIBLE');
    else writeln ('IMPOSSIBLE');
  end;
end.
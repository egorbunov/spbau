//AL: max number of different words
var i, k, written:integer;
    s:string;
begin
  randseed:=239932;
  writeln (8);
  s:='A';
  written:=3;
  repeat
    inc (written, length (s)+2);
    if written>20000 then halt;
    writeln (s);
    k:=0;
    for i:=length (s) downto 1 do 
      if s[i]<'Z' then begin inc (s[i]); k:=i; break end else s[i]:='A';
    if k=0 then s:=s+'A';
  until false;
end.
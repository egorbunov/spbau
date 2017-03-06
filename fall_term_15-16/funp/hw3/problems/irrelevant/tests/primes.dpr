var f:array [0..10000] of boolean;
    p:array [1..10000] of int64;
    i, j, cnt:integer;
    s:int64;

begin
  fillchar (f, sizeof (f), 1);
  f[0]:=false; f[1]:=false; cnt:=0;
  for i:=2 to high (f) do begin
    if f[i] then begin
      j:=i+i;
      while j<=high (f) do begin
        f[j]:=false; inc (j, i);
      end;
      inc (cnt);
      p[cnt]:=i;
    end;
  end;
  for i:=1 to 1000 do writeln (p[i]);
end.
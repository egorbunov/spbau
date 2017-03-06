var f:array [0..100000] of boolean;
    p:array [1..100000] of int64;
    i, j, n, cnt:integer;
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
  n:=100000;
  repeat
    for i:=1 to cnt do begin
      if p[i]*p[i]>n then begin writeln (n); halt end;
      if n mod p[i] = 0 then break
    end;
    dec (n);
  until false;
end.
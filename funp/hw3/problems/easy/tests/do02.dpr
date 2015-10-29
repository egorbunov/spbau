const 
  k = 137;
var
  i: Integer;
  p: Double;
begin 
  RandSeed := 1;
  WriteLn('13 27 ', k);
  p := 0.13;
  for i := 1 to k do begin
    WriteLn(p:0:2);
    p := p + (Random(10) + 1) * 0.01;
  end;
end.

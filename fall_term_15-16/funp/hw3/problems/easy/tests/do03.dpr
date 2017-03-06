const 
  k = 568;
var
  i: Integer;
  p: Double;
begin 
  RandSeed := 1;
  WriteLn('1 2 ', k);
  p := 99.99;
  for i := 1 to k do begin
    WriteLn(p:0:2);
    p := p - (Random(30) + 1) * 0.01;
  end;
end.

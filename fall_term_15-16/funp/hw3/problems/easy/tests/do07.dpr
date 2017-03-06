const 
  k = 10000;
var
  i: Integer;
begin 
  RandSeed := 1;
  WriteLn('1 2 ', k);
  for i := 1 to k do begin
    if i mod 2 = 0 then
      WriteLn('0.01')
    else
      WriteLn('99.99');
  end;
end.

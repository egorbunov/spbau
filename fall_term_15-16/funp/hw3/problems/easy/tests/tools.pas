unit tools;
interface

procedure Generate(seed: Integer; m, n, k: Integer; p, expect_yield, sigma_yield: Double);

implementation
uses
  Math;

procedure Generate(seed: Integer; m, n, k: Integer; p, expect_yield, sigma_yield: Double);
var
  i: Integer;
begin
  RandSeed := seed;
  WriteLn(m, ' ', n, ' ', k);
  for i := 1 to k do begin
    WriteLn(p:0:2);
    p := p * (1 + RandG(expect_yield, sigma_yield));
    if p > 99.99 then
      p := 99.99;
    if p < 0.01 then
      p := 0.01;
  end;
end;

end.

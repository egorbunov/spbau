unit Tools;

interface

procedure GenRandom(seed: Integer; adjust: Integer);
procedure GenRandom2(seed: Integer; adjust: Integer);

implementation

procedure GenRandomXYZ(x, y, z: Integer; adjust: Integer);
var 
  w, h: array[1..6] of integer;
  a, b, c, i, j: Integer;
begin
  w[1] := x;
  h[1] := y;
  w[2] := x;
  h[2] := y;
  w[3] := x;
  h[3] := z;
  w[4] := x;
  h[4] := z;
  w[5] := y;
  h[5] := z;
  w[6] := y;
  h[6] := z;

  // Random flips
  for i := 1 to 6 do
    if Random(2) = 0 then begin
      a := w[i];
      w[i] := h[i];
      h[i] := a;
    end;
  // Shuffle
  for i := 2 to 6 do begin
    j := Random(i) + 1;
    // swap i <-> j
    a := w[i];
    b := h[i];
    w[i] := w[j];
    h[i] := h[j];
    w[j] := a;
    h[j] := b;
  end;
  // Adjust specified number of times
  for i := 1 to adjust do 
    if Random(2) = 0 then
      w[Random(6) + 1] := Random(10000) + 1
    else
      h[Random(6) + 1] := Random(10000) + 1;
  // Print
  for i := 1 to 6 do
    WriteLn(w[i], ' ', h[i]);
end;

procedure GenRandom(seed: Integer; adjust: Integer);
var
  x, y, z: Integer;
begin
  RandSeed := seed;
  x := Random(10000) + 1;
  y := Random(10000) + 1;
  z := Random(10000) + 1;
  GenRandomXYZ(x, y, z, adjust);
end;

procedure GenRandom2(seed: Integer; adjust: Integer);
var
  x, y, z: Integer;
begin
  RandSeed := seed;
  x := Random(10000) + 1;
  y := Random(10000) + 1;
  z := y;
  GenRandomXYZ(x, y, z, adjust);
end;

end.
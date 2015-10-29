(*
    Solution for NEERC'2004 Problem K: Kingdom of Magic
    (C) Roman Elizarov
    Note: this solution if ineffective (uses Flyod algorithm)
    but it also prints to console a list of farthest points for the
    given input.
*)

{$APPTYPE CONSOLE}
{.$R+,Q+,S+,I+}
{$R-,Q-,S-,I+}

program Kingdom;
uses
  SysUtils;

const
  MAXN = 100;
  MAXM = 1000;

var
  n, m, a1, b1, a2, b2: Integer;
  i, j, k, a, b, maxdist: Integer;
  changes: Boolean;
  v1, v2: Integer;
  va, vb: array[1..2*MAXM] of Integer;
  g: array[1..MAXN, 1..MAXN] of Boolean;
  h, hp: array[1..2*MAXM, 1..2*MAXM] of Integer; 

procedure PrintWithDist(dist: Integer);
var 
  i, j: Integer;
begin
  for i := 1 to 2*m do 
    for j := 1 to i - 1 do
      if h[i, j] = dist then
        WriteLn(dist, ': ', va[i], ' ', vb[i], ' -> ', va[j], ' ', vb[j]);
end;

function CountPairs(v1, v2: Integer): Integer;
var 
  vp: Integer;
begin
  vp := hp[v1, v2];
  if vp <> 0 then
    Result := CountPairs(v1, vp) + 1 + CountPairs(vp, v2)
  else
    Result := 0;
end;

procedure PrintPairs(v1, v2: Integer);
var 
  vp: Integer;
begin
  vp := hp[v1, v2];
  if vp <> 0 then begin
    PrintPairs(v1, vp);
    WriteLn(va[vp], ' ', vb[vp]);
    PrintPairs(vp, v2);
  end;
end;

begin
  // Read input
  Assign(input, 'kingdom.in');
  Reset(input);

  ReadLn(n, m, a1, b1, a2, b2);
  for i := 1 to m do begin
    ReadLn(a, b);
    va[2*i - 1] := a;
    vb[2*i - 1] := b;
    va[2*i] := b;
    vb[2*i] := a;
    g[a, b] := true;
    g[b, a] := true;
  end;

  // Initialize initial distance matrix
  for i := 1 to 2*m do 
    for j := 1 to 2*m do 
      if i = j then
        h[i, j] := 0
      else if (va[i] = va[j]) and g[vb[i], vb[j]] then
        h[i, j] := 1
      else if (vb[i] = vb[j]) and g[va[i], va[j]] then
        h[i, j] := 1
      else if g[va[i], va[j]] and g[vb[i], vb[j]] and ((va[i] <> vb[j]) or (vb[i] <> va[j])) then
        h[i, j] := 2
      else
        h[i, j] := MaxInt;

  // Update distances with Floyd algorithm
  for k := 1 to 2*m do
    for i := 1 to 2*m do
      if h[i, k] <> MaxInt then
        for j := 1 to 2*m do
          if (h[k, j] <> MaxInt) and (h[i, k] + h[k, j] < h[i, j]) then begin
            h[i, j] := h[i, k] + h[k, j];
            hp[i, j] := k;
          end;

  // Now determine max distance
  maxdist := -1;
  for i := 1 to 2*m do
    for j := 1 to i - 1 do
      if (h[i, j] <> MaxInt) and (h[i, j] > maxdist) then
        maxdist := h[i, j];

  PrintWithDist(maxdist);
  PrintWithDist(maxdist - 1);


  // Write answer
  Assign(output, 'kingdom.out');
  Rewrite(output);

  v1 := 0;
  v2 := 0;
  for i := 1 to 2*m do begin
    if (va[i] = a1) and (vb[i] = b1) then
      v1 := i;
    if (va[i] = a2) and (vb[i] = b2) then
      v2 := i;
  end;
  Assert(v1 <> 0, 'a1, b1 invalid');
  Assert(v2 <> 0, 'a2, b2 invalid');
  Assert(h[v1, v2] <> MaxInt, 'Solution does not exist');

  WriteLn(h[v1, v2], ' ', CountPairs(v1, v2) + 2);
  WriteLn(a1, ' ', b1);
  PrintPairs(v1, v2);
  WriteLn(a2, ' ', b2);
end.

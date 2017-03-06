(*
    Solution for NEERC'2004 Problem E: Easy Trading
    (C) Roman Elizarov
    Note: this solution attempts to check correctness of the input
*)

{$APPTYPE CONSOLE}
{$R+,Q+,S+,I+}
program Easy;
uses
  SysUtils;

const
  MAXN = 100;
  MAXK = 10000;

var
  s: String;
  m, n, k, i, code, last, cur: Integer;
  d: Double;
  p: array[1..MAXK] of Integer; // original prices x100
  pp: array[0..MAXK] of Integer; // partial sums for p

begin
  // Input and output
  Assign(input, 'easy.in');
  Reset(input);
  Assign(output, 'easy.out');
  Rewrite(output);

  // Read
  ReadLn(m, n, k);
  Assert((m >= 1) and (n > m) and (n <= MAXN) and (k > n) and (k <= MAXK), 'Sizes out of range');

  pp[0] := 0; // partial sum
  for i := 1 to k do begin
    ReadLn(s);
    // Make sure we have 2 digits
    Assert(Pos('.', s) = Length(s) - 2, 'Price does not have 2 digits after decimal point');
    Val(s, d, code);
    Assert((code = 0) and (d > 0) and (d < 100), 'Price is out of range of invalid');
    p[i] := Round(100 * d);
    pp[i] := p[i] + pp[i - 1];
  end;
  Assert(SeekEof, 'Extra data in file');

  // Process
  last := 0;
  for i := n to k do begin
    cur := (pp[i] - pp[i - m]) * n - (pp[i] - pp[i - n]) * m;
    Assert(cur <> 0, 'Averages are equal');
    if (cur > 0) and (last <= 0) then
      WriteLn('BUY ON DAY ', i)
    else if (cur < 0) and (last >= 0) then
      WriteLn('SELL ON DAY ', i);  
    last := cur;
  end;
end.

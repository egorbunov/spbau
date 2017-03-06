uses dorand, tools;
type integer=longint;
begin
  regint (30);
  Init (1000);
  GenPosDistr (lnd);
  Create;
  SmallLie (1, 100);
  Display;
end.
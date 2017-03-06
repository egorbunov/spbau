uses dorand, tools;
type integer=longint;
begin
  regint (31);
  Init (1000);
  GenPosDistr (fvd);
  Create;
  SmallLie (1, 100);
  Display;
end.
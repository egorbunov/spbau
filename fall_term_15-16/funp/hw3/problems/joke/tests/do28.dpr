uses dorand, tools;
type integer=longint;
begin
  regint (28);
  Init (1000);
  GenPosPoisson (5);
  Create;
  SmallLie (1, 100);
  Display;
end.
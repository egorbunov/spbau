uses dorand, tools;
type integer=longint;
begin
  regint (29);
  Init (1000);
  GenPosPoisson (1);
  Create;
  SmallLie (1, 100);
  Display;
end.
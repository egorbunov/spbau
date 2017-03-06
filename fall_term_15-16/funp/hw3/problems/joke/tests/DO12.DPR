uses dorand, tools;
type integer=longint;
begin
  regint (11);
  Init (100);
  GenPosPoisson (1);
  Create;
  HugeLie (1, 5);
  SmallLie (1, 5);
  Display;
end.
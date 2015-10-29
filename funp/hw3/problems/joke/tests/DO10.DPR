uses dorand, tools;
type integer=longint;
begin
  regint (9);
  Init (20);
  GenPosPoisson (2);
  Create;
  SmallLie (1, 4);
  Display;
end.
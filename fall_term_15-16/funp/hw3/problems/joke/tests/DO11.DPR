uses dorand, tools;
type integer=longint;
begin
  regint (10);
  Init (35);
  GenPosPoisson (1.3);
  Create;
  HugeLie (1, 3);
  Display;
end.
uses dorand, tools;
type integer=longint;
begin
  regint (12);
  Init (239);
  GenPosPoisson (1.1);
  Create;
  Save;
  GenPosPoisson (1.2);
  Create;
  Merge (1, 1);
  Display;
end.
uses dorand, tools;
type integer=longint;
begin
  regint (24);
  Init (1000);
  GenPosPoisson (100);
  Create;
  Save;
  GenPosPoisson (100);
  Create;
  Merge (9, 11);
  Display;
end.
uses dorand, tools;
type integer=longint;
begin
  regint (23);
  Init (999);
  GenPosPoisson (2);
  Create;
  Save;
  GenPosPoisson (2);
  Create;
  Merge (1, 1);
  Display;
end.
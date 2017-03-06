uses dorand, tools;
type integer=longint;
begin
  regint (13);
  Init (500);
  GenPosDistr (sqd);
  Create;
  Save;
  GenPosPoisson (1.25);
  Create;
  Merge (28, 32);
  Display;
end.
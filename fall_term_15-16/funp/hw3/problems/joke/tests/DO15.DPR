uses dorand, tools;
type integer=longint;
begin
  regint (14);
  Init (1000);
  GenPosDistr (lnd);
  Create;
  Save;
  GenPosDistr (sqd);
  Create;
  Merge (1, 1);
  Save;
  GenPosPoisson (1);
  Create;
  Merge (2, 1);
  Display;
end.
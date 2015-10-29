uses dorand, tools;
type integer=longint;
function std:extended;begin std:=fvd*fvd end;
begin
  regint (33);
  Init (1000);
  GenPosDistr (std);
  Create;
  Save;
  GenPosPoisson (1);
  Create;
  Merge (1, 1);
  Display;
end.
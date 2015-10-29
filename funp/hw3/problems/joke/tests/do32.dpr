uses dorand, tools;
type integer=longint;
function std:extended;begin std:=fvd*fvd end;
begin
  regint (32);
  Init (1000);
  GenPosDistr (std);
  Create;
  SmallLie (1, 100);
  Display;
end.
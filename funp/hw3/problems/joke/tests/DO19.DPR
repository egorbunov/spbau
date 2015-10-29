uses dorand, tools;
type integer=longint;
function std:extended; begin std:=0 end;
begin
  regint (18);
  Init (1000);
  GenPosDistr (std);
  Create;
  SmallLie (2, 3);
  Display;
end.
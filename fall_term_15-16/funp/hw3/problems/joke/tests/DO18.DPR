uses dorand, tools;
type integer=longint;
function std:extended; begin std:=0.9999999 end;
begin
  regint (17);
  Init (1000);
  GenPosDistr (std);
  Create;
  HugeLie (2, 3);
  Display;
end.
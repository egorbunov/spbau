uses dorand, tools;
type integer=longint;
function std:extended; begin std:=0 end;
begin
  regint (16);
  Init (1000);
  GenPosDistr (std);
  Create;
  Display;
end.
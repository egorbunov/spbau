unit tools;

interface

const len = 100;
function randomstr( l: integer ): string;
function reorderstr( s: string ): string;

implementation

function randomstr( l: integer ): string;
var i: integer;
    s: string;
begin
  s := '';
  for i := 1 to l do s := s + chr(random(26) + ord('A'));
  randomstr := s;
end;

function reorderstr( s: string ): string;
var i,j,p,cnt: integer;
    used: array [1..len] of boolean;
begin
  fillchar( used, sizeof(used), false );
  result := '';

  for i := 1 to length(s) do begin
    cnt := 0;
    p := random(length(s)-i+1);
    for j := 1 to length(s) do 
      if not used[j] then begin
        if cnt = p then begin
          used[j] := true;
          result := result + s[j];
          break;
        end;
        inc(cnt);
      end;
  end;
end;

begin
end.
{$apptype CONSOLE}

type signature = array [0..100] of byte;

procedure count( s: string; var sig: signature );
var i: integer; c: char;
    subst: array ['A'..'Z'] of byte;
begin
  fillchar( sig, sizeof(sig), 0 );
  fillchar( subst, sizeof(subst), 0 );

  for i := 1 to length(s) do
    inc(subst[s[i]]);

  for c := 'A' to 'Z' do begin
    inc(sig[subst[c]]);
  end;
end;

var one,two: string;
    onesig,twosig: signature;
    i: integer;

begin
  assign(input,'ancient.in');
  reset(input);
  assign(output,'ancient.out');
  rewrite(output);

  readln(one);
  readln(two);

  count(one,onesig);
  count(two,twosig);

  for i := 1 to 100 do
    if onesig[i] <> twosig[i] then begin writeln('NO'); halt(0); end;

  writeln('YES');
end.


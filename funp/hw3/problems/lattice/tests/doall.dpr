var
  s, tn: String;
  i: Integer;

function SkipNum(s: String; i: Integer): Integer;
begin
  while (i <= Length(s)) and (s[i] = ' ') do
    Inc(i);
  while (i <= Length(s)) and (s[i] >= '0') and (s[i] <= '9') do
    Inc(i);
  result := i;
end;

begin
  Reset(input, 'tests.lst');
  while not SeekEof do begin
    ReadLn(s);
    tn := Copy(s, 1, 2);
    Delete(s, 1, 3);
    i := SkipNum(s, 1);
    i := SkipNum(s, i);
    i := SkipNum(s, i);
    Rewrite(output, tn);
    WriteLn(Copy(s, 1, i - 1));
    Close(output);
  end;
end.

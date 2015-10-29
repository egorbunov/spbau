{$O-,Q+,R+}
const Letters=['A'..'Z'];

var t1, t2:array ['A'..'Z'] of integer;
    i, tmp:integer;
    c, d:char;
    s1, s2:string;


begin
  assign (input, 'ancient.in'); reset (input);
  assign (output, 'ancient.out'); rewrite (output);
  readln (s1); for i:=1 to length (s1) do assert (s1[i] in Letters);
  readln (s2); for i:=1 to length (s2) do assert (s2[i] in Letters);
  assert (seekeof); assert (length (s1)=length (s2));
  assert ((length (s1) in [1..100])); //empty line is not allowed by background
  for i:=1 to length (s1) do inc (t1[s1[i]]);
  for i:=1 to length (s2) do inc (t2[s2[i]]);
  for c:='A' to 'Z' do begin
    for d:='Z' downto succ (c) do begin
      if t1[d]<t1[pred (d)] then 
        begin tmp:=t1[d]; t1[d]:=t1[pred (d)]; t1[pred (d)]:=tmp end;
      if t2[d]<t2[pred (d)] then
        begin tmp:=t2[d]; t2[d]:=t2[pred (d)]; t2[pred (d)]:=tmp end;
    end;
    if t1[c]<>t2[c] then begin writeln ('NO'); halt end;
  end;
  writeln ('YES');
end.
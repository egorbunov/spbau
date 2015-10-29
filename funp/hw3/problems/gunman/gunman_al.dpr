{$O-,Q+,R+}
uses SysUtils;

const MaxN=100;
      MaxC=1000;

type CArr=array [1..MaxN] of integer;
     num=extended;
     rational=record p, q:num; end;


function less (var a, b:rational):boolean;
begin
  less:=a.p*b.q<b.p*a.q;
end;

var n:integer;

procedure planar (var x1, x2, y:CArr; var rmin, rmax, xp, yp:rational);
var mind, maxd:rational;

procedure test (cx1, cy1, cx2, cy2:int64);
var a, b, c:num;
    k:integer;
    x:rational;
begin
  a:=cy2-cy1; //always >0
  b:=cx1-cx2;
  c:=cx2*cy1-cx1*cy2;
  for k:=1 to n do 
    if (a*x1[k]+b*y[k]+c>0) or (a*x2[k]+b*y[k]+c<0) then exit;
  x.p:=-c; x.q:=a;
  if less (x, rmin) then begin rmin:=x; mind.p:=-b; mind.q:=a end;
  if less (rmax, x) then begin rmax:=x; maxd.p:=-b; maxd.q:=a end;
end;

var i, j:integer;
    a1, b1, c1, a2, b2, c2:num;

begin
  rmin.p:=maxint; rmin.q:=1; rmax.p:=-maxint; rmax.q:=1;
  for i:=1 to n do
    for j:=i+1 to n do
      if j<>i then begin
        test (x1[i], y[i], x1[j], y[j]);
        test (x1[i], y[i], x2[j], y[j]);
        test (x2[i], y[i], x1[j], y[j]);
        test (x2[i], y[i], x2[j], y[j]);
      end;
  if rmin.p*rmax.q=rmin.q*rmax.p then begin
    xp.p:=mind.p*rmax.q+rmax.p;
    xp.q:=rmax.q;
    yp.p:=mind.q;
    yp.q:=1
  end else
  if not less (rmin, rmax) then begin
    writeln ('UNSOLVABLE');
    halt
  end else
  begin
    a1:=mind.q; b1:=-mind.p; c1:=-rmin.p;
    a2:=maxd.q; b2:=-maxd.p; c2:=-rmax.p;
    xp.q:=a1*b2-a2*b1; xp.p:=b1*c2-b2*c1;
    if xp.q<0 then begin xp.q:=-xp.q; xp.p:=-xp.p end;
    yp.q:=a1*b2-a2*b1; yp.p:=a2*c1-a1*c2;
    if yp.q<0 then begin yp.q:=-yp.q; yp.p:=-yp.p end;
  end;
end;



var x1, y1, x2, y2, z:CArr;
    dx, dy, dz, dz2:num;
    i:integer;
    xmin, xmax, ymin, ymax, xpx, ypy, zpx, zpy:rational;

begin
  assign (input, 'gunman.in'); reset (input);
  assign (output, 'gunman.out'); rewrite (output);
  assert (not seekeoln); read (n); assert ((n>=2) and (n<=MaxN));
  assert (seekeoln); readln;
  for i:=1 to n do begin
    assert (not seekeoln); read (x1[i]);
    assert (not seekeoln); read (y1[i]);
    assert (not seekeoln); read (x2[i]);
    assert (not seekeoln); read (y2[i]);
    assert (not seekeoln); read (z[i]);
    assert ((x1[i]>0) and (x1[i]<MaxC) and
            (x2[i]>0) and (x2[i]<MaxC) and
            (y1[i]>0) and (y1[i]<MaxC) and
            (y2[i]>0) and (y2[i]<MaxC) and
            (z[i]>0) and (z[i]<MaxC) and 
            (x1[i]<x2[i]) and (y1[i]<y2[i]));
    assert (seekeoln); readln;
  end;
  for i:=2 to n do assert (z[i]>z[i-1]);
  assert (seekeof);
  planar (x1, x2, z, xmin, xmax, xpx, zpx);
  planar (y1, y2, z, ymin, ymax, ypy, zpy);
  if (ymin.p>0) or (ymax.p<0) then begin writeln ('UNSOLVABLE'); halt end;
  //Substitute min with max to maximize precision loss. This solution 
  //minimizes it :)
  dx:=(xpx.p*xmin.q-xmin.p*xpx.q)*zpx.q;
  dz:=zpx.p*xpx.q*xmin.q;
  dy:=ypy.p*zpy.q;
  dz2:=zpy.p*ypy.q;
  dx:=dx*dz2; dy:=dy*dz; dz:=dz*dz2;
  writeln ('SOLUTION');
  writeln (xmin.p/xmin.q:0:20);
  for i:=1 to n do
    writeln (dx*z[i]/dz+xmin.p/xmin.q:0:20, ' ', dy*z[i]/dz:0:20, ' ', z[i]);
end.
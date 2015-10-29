{$R+,Q+,C+,O-}
{$apptype CONSOLE}

uses
	SysUtils, Math;

const
	IN_FILE = 'find.in';
	OUT_FILE = 'find.out';

	MAX_N = 100;
	MAX_M = 3 * MAX_N;
	MAX_C = 100;

	EPSILON = 1e-10;
    INFINITY = 1e10;

type
	real = extended;

	TVector = record
		x, y : integer;		
	end;
			
	TRealVector = record
		x, y : real;		
	end;
			
var
	n : integer;
	inp : array[0..MAX_N + 1] of TVector;
	
	m : integer;
	outp : array[1..MAX_M] of TRealVector;

procedure ReadInput;
var
	i : integer;
	
begin
	reset(input, IN_FILE);
	read(n);
	assert((n >= 3) and (n <= MAX_N));
	assert(eoln);
	readln;
	for i := 1 to n do begin
		read(inp[i].x);
		assert((inp[i].x >= 0) and (inp[i].x <= MAX_C));
		assert(not eoln);
		read(inp[i].y);
		assert((inp[i].y >= 0) and (inp[i].y <= MAX_C));
		assert(eoln);
		readln;
	end;
    assert(eof);
	close(input);

	inp[n + 1] := inp[1];
    inp[0] := inp[n];
end;

function GetPoint(const a : TVector; const d : TVector; t : real) : TRealVector;
begin
	result.x := a.x + t * d.x;
	result.y := a.y + t * d.y;
end;

function GetEdge(i : integer) : TVector;
begin
	result.x := inp[i + 1].x - inp[i].x;
	result.y := inp[i + 1].y - inp[i].y;
end;

function AreEqual(const lhs, rhs : TRealVector) : boolean; overload;
begin
	AreEqual := (abs(lhs.x - rhs.x) < EPSILON) and (abs(lhs.y - rhs.y) < EPSILON);
end;

function AreEqual(const lhs, rhs : TVector) : boolean; overload;
begin
	AreEqual := (lhs.x = rhs.x) and (lhs.y = rhs.y);
end;

function Negate(const v : TVector) : TVector;
begin
	result.x := -v.x;
    result.y := -v.y;
end;

function det(a, b, c, d : integer) : integer;
begin
	result := a * d - b * c;
end;

function Intersect(const a1, d1 : TVector; const a2, d2 : TVector; var t1, t2 : real) : boolean;
var
	q, q1, q2 : integer;
	
begin
	q  := det(d1.x, -d2.x, d1.y, -d2.y);
	q1 := det(a2.x - a1.x, -d2.x, a2.y - a1.y, -d2.y);
	q2 := det(d1.x, a2.x - a1.x, d1.y, a2.y - a1.y);

    if q < 0 then begin
    	q := -q;
        q1 := -q1;
        q2 := -q2;
    end;

	if (q <> 0) and (q1 >= 0) and (q2 >= 0) and (abs(q1) <= abs(q)) and (abs(q2) <= abs(q)) then begin
		t1 := q1 / q;
		t2 := q2 / q;
		result := true;
	end else
		result := false;
end;

procedure Advance(const a : TVector; const d : TVector; var t : real);
var
	t1, t2, mint : real;
	i : integer;

begin
	mint := INFINITY;
    for i := 1 to n do
    	if Intersect(a, d, inp[i], GetEdge(i), t1, t2) then begin
            if (t1 > t + EPSILON) and (t1 < mint) then
            	mint := t1;
	    end;
    t := mint;
end;

function CrossProduct(const a, b : TVector) : integer;
begin
	result := a.x * b.y - a.y * b.x;
end;

function GetAngle(const a, b : TVector) : real;
begin
	result := arccos((a.x * b.x + a.y * b.y) / sqrt(sqr(a.x) + sqr(a.y)) / sqrt(sqr(b.x) + sqr(b.y)));
    if CrossProduct(a, b) < 0 then
    	result := -result;
end;

procedure Rotate(var a : TVector; var d : TVector; var t : real);
var
	t1, t2 : real;
	i : integer;

	bestphi : real;
    besta, bestd : TVector;
    bestt : real;

    procedure Take(const thisa, thisd : TVector; thist : real);
    var
    	phi : real;

    begin
		phi := GetAngle(d, thisd);
//writeln('take (', thisa.x, ',', thisa.y, '), dir (', thisd.x, ',', thisd.y, '), t = ', thist:0:3, ', phi = ', phi/pi*180:0:3);
        if phi < bestphi then begin
			bestphi := phi;
            besta := thisa;
            bestd := thisd;
            bestt := thist;
        end;
    end;

begin
	bestphi := INFINITY;
    
    for i := 1 to n do
    	if Intersect(a, d, inp[i], GetEdge(i), t1, t2) then begin
            if abs(t1 - t) < EPSILON then begin
            	if t2 < 1 - EPSILON then
                	Take(inp[i], GetEdge(i), t2);
            	if t2 > EPSILON then
                	Take(inp[i+1], Negate(GetEdge(i)), 1 - t2);
            end;
	    end;

    a := besta;
    d := bestd;
    t := bestt;
end;

procedure GetStart(var a : TVector; var d : TVector; var t : real);
var
	i, j : integer;

begin
	j := 1;
	for i := 1 to n do
		if (inp[i].y < inp[j].y) or ((inp[i].y = inp[j].y) and (inp[i].x < inp[j].x)) then
			j := i;

	a := inp[j];
	t := 0;
	if false{CrossProduct(GetEdge(j), Negate(GetEdge(j - 1))) > 0} then
	    d := GetEdge(j)
    else
    	d := Negate(GetEdge(j - 1));
end;

procedure Solve;
var
	a, d : TVector;
	t : real;

begin
	GetStart(a, d, t);
	outp[1] := GetPoint(a, d, t);
	m := 1;
	repeat
//writeln('solve (', a.x, ',', a.y, '), dir (', d.x, ',', d.y, '), t = ', t:0:3);
		Advance(a, d, t);
		outp[m + 1] := GetPoint(a, d, t);
		if AreEqual(outp[m + 1], outp[1]) then
			break;
		inc(m);
		Rotate(a, d, t);
	until false;
end;

procedure WriteOutput;
var
	i : integer;

begin
	rewrite(output, OUT_FILE);
	writeln(m);
	for i := 1 to m do
		writeln(outp[i].x:0:4, ' ', outp[i].y:0:4);
	close(output);
end;

procedure Check;
var
	i, j : integer;
    t1, t2 : real;
    
begin
	for i := 1 to n do
    	for j := i + 1 to n do
        	assert(not AreEqual(inp[i], inp[j]));

    for i := 1 to n do
    	assert(CrossProduct(GetEdge(i), GetEdge(i - 1)) <> 0);

    for i := 1 to n do
    	for j := 1 to n do
        	if Intersect(inp[i], GetEdge(i), inp[j], GetEdge(j), t1, t2) then begin
    			assert(not ((abs(t1) < EPSILON) and (abs(t2) > EPSILON) and (abs(t2 - 1) > EPSILON)));
	        end;
end;

begin
	ReadInput;
    Check;
	Solve;
    WriteOutput;
end.

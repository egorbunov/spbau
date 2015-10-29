(*
    Checker for NEERC'2004 Problem G: Gunman
    (C) Georgiy Korneev, Maxim Babenko, Roman Elizarov
*)

{$R+,Q+,C+,O-}
{$apptype CONSOLE}

uses
    testlib, sysutils, math;

const
    MAX_N = 100;
    EPSILON = 5e-4;

    SOLUTION = 'SOLUTION';
    UNSOLVABLE = 'UNSOLVABLE';

type
    real = extended;

var
    ja, pa: string;

    i: integer;

    n: integer;
    x, y: array[1..2,1..MAX_N] of integer;
    z: array[1..MAX_N] of integer;

    xp, yp, zp: array[0..MAX_N] of real;

    y2, yz, z2: real;
    xz: real;

    alpha, beta: real;
    dx, dy: real;

begin
    // Read input

    n := inf.ReadInteger;
    for i := 1 to n do begin
    	x[1,i] := inf.readInteger;
        y[1,i] := inf.readInteger;
        x[2,i] := inf.ReadInteger;
        y[2,i] := inf.ReadInteger;
        z[i] := inf.ReadInteger;
    end;

    // Read output and check for all PEs

    pa := trim(ouf.readString());
    if (pa <> SOLUTION) and (pa <> UNSOLVABLE) then begin
        quit(_pe, format('"SOLUTION" or "UNSOLVABLE" expected instead of "%s"', [pa]));
    end;

    // If participant reports SOLUTION, then read it to catch any PEs first.
    // We purposefully do not perform strict line-by-line check of the output
    // and allow it to be quite free-form.

    if pa = SOLUTION then begin
        xp[0] := ouf.readReal;
        yp[0] := 0;
        zp[0] := 0;

        for i := 1 to n do begin
            xp[i] := ouf.readReal;
            yp[i] := ouf.readReal;
            zp[i] := ouf.readReal;
        end;
    end;

    if not ouf.seekEof then 
      quit(_pe, 'Extra information in the output file');

    // Done with PE checks. Now read correct answer and check if
    // participant's answer is correct.

    ja := ans.readString();

    if (pa = UNSOLVABLE) and (ja = SOLUTION) then
        quit(_wa, 'Solution exists but was not found');

    if (pa = UNSOLVABLE) and (ja = UNSOLVABLE) then
        quit(_ok, format('N = %d: UNSOLVABLE', [n]));

    for i := 1 to n do begin
        if (abs(zp[i] - z[i]) > EPSILON) or
           (xp[i] < x[1,i] - EPSILON) or (xp[i] > x[2,i] + EPSILON) or
           (yp[i] < y[1,i] - EPSILON) or (yp[i] > y[2,i] + EPSILON) then
        	quit(_wa, format('Point %d is out of window', [i]));
    end;

    // Find best-fitting line

    y2 := 0;
    for i := 1 to n do
    	y2 := y2 + yp[i] * yp[i];

    z2 := 0;
    for i := 1 to n do
    	z2 := z2 + zp[i] * zp[i];

    yz := 0;
    for i := 1 to n do
    	yz := yz + yp[i] * zp[i];

    xz := 0;
    for i := 1 to n do
    	xz := xz + (xp[i] - xp[0]) * zp[i];

    alpha := xz / z2;
    beta  := yz / z2;

    // Check this line

    for i := 0 to n do begin
    	dx := xp[0] + alpha * zp[i] - xp[i];
    	if abs(dx) > EPSILON then
        	quit(_wa, format('Points are not collinear: check for %d failed with xdiff = %.6f', [i, dx]));
    	dy := beta * zp[i] - yp[i];
    	if abs(dy) > EPSILON then
        	quit(_wa, format('Points are not collinear: check for %d failed with ydiff = %.6f', [i, dy]));
    end;

    if ja = UNSOLVABLE then
        quit(_fail, 'Solution exists but was not found by jury');

    quit(_ok, format('N = %d: SOLUTION', [n]));
end.

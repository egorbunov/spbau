(*
    Checker for NEERC'2004 Problem K: Kingdom of Magic
    (C) Georgiy Korneev, Roman Elizarov
*)

uses
    testlib, sysutils;

const
    max_n = 100;
    max_m = 5000;
    max_l = max_m * 10;

var
    n, m: longint;
    a1, b1, a2, b2: longint;
    a, b: array [1..max_l] of longint;
    c: array [1..max_n, 1..max_n] of longint;
    v1, v2: longint;
    pe, pl, pc, je: longint;

procedure readInput();
var
	i: longint;
begin
    n := inf.readLongint();
    m := inf.readLongint();

    a1 := inf.readLongint();
    b1 := inf.readLongint();
    a2 := inf.readLongint();
    b2 := inf.readLongint();

    for i := 1 to m do begin
        v1 := inf.readLongint();
        v2 := inf.readLongint();
        c[v1][v2] := i;
        c[v2][v1] := i;
    end;
end;

procedure readOutput();
var
    i, ai, bi: longint;
begin
    pe := ouf.readLongint();
    pl := ouf.readLongint();

    for i := 1 to pl do begin
    	ai := ouf.readLongint();
    	bi := ouf.readLongint();
    	if i <= max_l then begin
    	  a[i] := ai;
    	  b[i] := bi;
    	end;
    end;

    if not ouf.seekEof then begin
        quit(_pe, 'Extra information in the output file');
    end;

    if pl > max_l then begin
    	quit(_wa, format('Too many moves (%d)', [pl]));
    end;
end;

procedure checkBounds(a, b, x: longint; message: string);
begin
    if (x < a) or (b < x) then begin
        quit(_wa, format(message, [x]));
    end;
end;

var
    i: longint;
begin
    readInput();
    readOutput();

    if (a1 <> a[1]) or (b1 <> b[1]) then begin
        quit(_wa, format('Initial position (%d, %d) instead of (%d, %d)', [a[1], b[1], a1, b1]));
    end;

    pc := 0;
    for i := 2 to pl do begin
        checkBounds(1, n, a[i], format('Step %d: A (%%d) out of bounds', [i]));
        checkBounds(1, n, b[i], format('Step %d: B (%%d) out of bounds', [i]));

        if c[a[i]][b[i]] = 0 then begin
            quit(_wa, format('Step %d: Towns %d and %d are not neigboring', [i, a[i], b[i]]));
        end;

        if (a[i - 1] = a[i]) and (b[i - 1] = b[i]) then begin
            quit(_wa, format('Step %d: Stay at (%d, %d)', [i, a[i - 1], b[i - 1]]));
        end;
        if (a[i - 1] = b[i]) and (b[i - 1] = a[i]) then begin
            quit(_wa, format('Step %d: Gate is used simultaneously (%d, %d)', [i, a[i - 1], b[i - 1]]));
        end;

        if (a[i - 1] <> a[i]) and (c[a[i - 1]][a[i]] = 0) or (b[i - 1] <> b[i]) and (c[b[i - 1]][b[i]] = 0) then begin
            quit(_wa, format('Step %d: Cannot move from (%d, %d) to (%d, %d)', [i, a[i - 1], b[i - 1], a[i], b[i]]));
        end;
        if (a[i - 1] <> a[i]) then inc(pc);
        if (b[i - 1] <> b[i]) then inc(pc);
    end;

    if (a[pl] <> a2) or (b[pl] <> b2) then begin
        quit(_wa, format('Finished at (%d, %d) instead of (%d, %d)', [b[pl], b[pl], a2, b2]));
    end;
    if (pe <> pc) then begin
        quit(_wa, format('Travel cost %d, promised %d', [pc, pe]));
    end;

    je := ans.readLongint();
    if (pe > je) then begin
        quit(_wa, format('Travel cost %d, instead of %d', [pe, je]));
    end;
    if (pe < je) then begin
        quit(_fail, format('Travel with cost %d found, instead of %d', [pe, je]));
    end;
    quit(_ok, format('Travel cost: %d; moves: %d', [pe, pl - 1]));
end.

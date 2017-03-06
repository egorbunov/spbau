(*
    Checker for NEERC'2004 Problem F: Find the Border
    (C) Georgiy Korneev, Roman Elizarov
*)

{$q+,r+,o-}
uses
    testlib, sysutils;

const
    max_n = 100;
    eps = 1.1e-4;
    max_list = max_n * max_n;

type
    tlist = array [1..max_list] of extended;

function rotate(var x, y: tlist; l: longint): longint;
var
    i, j: longint;
    tx, ty: tlist;
begin
    j := 1;
    for i := 2 to l do begin
        if (x[j] > x[i] + eps) or (abs (x[j] - x[i])<=eps) and (y[j] > y[i]) then begin
            j := i;
        end;
    end;

    for i := 1 to l do begin
        tx[(i - j + l) mod l + 1] := x[i];
        ty[(i - j + l) mod l + 1] := y[i];
    end;
    x := tx;
    y := ty;
    rotate := j - 1;
end;

var
    jx, jy, px, py: tlist;
    i, jc, pc, jr, pr: longint;
    x, y: extended;
begin
    // Read jury answer
    jc := ans.readLongint();
    for i := 1 to jc do begin
        jx[i] := ans.readReal();
        jy[i] := ans.readReal();
    end;

    // Read participant's answer (spit out all PEs)
    // Here we deliberately don't check line-by-line structure of output --
    // probles is complex by itself, so we are more flexible in allowed output format.
    pc := ouf.readLongint();
    for i := 1 to pc do begin
        x := ouf.readReal();
        y := ouf.readReal();
        if i <= max_list then begin
            px[i] := x;
            py[i] := y;
        end;
    end;
    if not ouf.seekEof then 
      quit(_pe, 'Extra information in the output file');

    // Now check if answer is correct
    if jc <> pc then begin
        quit(_wa, format('%d edges instead of %d', [pc, jc]));
    end;

    jr := rotate(jx, jy, jc);
    pr := rotate(px, py, jc);

    for i := 1 to jc do begin
        if (abs(jx[i] -  px[i]) > eps) or (abs(jy[i] -  py[i]) > eps) then begin
            quit(_wa, format('%d edge doesn''t match', [i]));
        end;
    end;

    quit(_ok, format('%d edges (output rotated by %d, answer rotated by %d)', [jc, pr, jr]));
end.
(*
    Checker for NEERC'2004 Problem C: Chandelier
    (C) Georgiy Korneev, Roman Elizarov
*)

{$MINSTACKSIZE $001000000}
{$MAXSTACKSIZE $001000000}
{$q+,r+,o-}
uses
    testlib, sysutils;
var
    o, s: string;
    p: integer;

(*
  Usage: min_depth := rec(orig_depth);
  Inputs: 
    s - original program
    o - pointer to the last command in the original program
  Outputs:
    o - Normalized original program
    min_depth -- minimal depth of the program (depth of normalized program)
    orig_depth -- depth of the original program

*)
function rec(out orig_depth: integer): integer;
var
    i, j, l: integer;
    t: array [1..9] of integer;
    q: array [1..9] of string;
    shift, deep, mdeep, odeep: integer;
    mo: string;
begin
    if p <= 0 then
        quit(_wa, 'Wrong program -- stack underflow');
    case s[p] of
        'a': begin
            rec := 1;
            orig_depth := 1;
            dec(p);
            o := 'a';
        end;
        '1'..'9': begin
            l := ord(s[p]) - ord('0');
            dec(p);
            orig_depth := 0;
            for i := 1 to l do begin
                t[i] := rec(odeep);
                q[i] := o;
                if odeep + l - i > orig_depth then
                    orig_depth := odeep + l - i;
            end;

            mdeep := maxint;
            shift := 0;
            mo := '';
            for i := 1 to l do begin
                deep := 0;
                o := '';
                for j := 1 to l do begin
                    o := o + q[(i + j) mod l + 1];
                    if deep < t[(i + j) mod l + 1] + l - j then begin
                        deep := t[(i + j) mod l + 1] + l - j;
                    end;
                end;
                if (mdeep > deep) or (mdeep = deep) and (mo > o) then begin
                    mdeep := deep;
                    shift := i;
                    mo := o;
                end;
            end;

            o := '';
            for j := 1 to l do begin
                o := q[(shift + j) mod l + 1] + o;
            end;
            rec := mdeep;
            o := o + chr(l + ord('0'));
        end;
        else begin
            quit(_wa, format('Unknown symbol "%s"', [s[p]]));
            rec := 0;
            orig_depth := 0;
        end;
    end;
end;

var
    oe, ac, id, od: integer;
    ip, op, ni, no: string;
    i: integer;
begin
    ip := trim(inf.readString());

    oe := ouf.readInteger();
    if not ouf.seekEoln then
        quit(_pe, 'Extra data on the first line');
    ouf.nextLine();
    op := trim(ouf.readString());

    for i := 1 to length(op) do begin
        if not (op[i] in ['a', '1'..'9']) then begin
            quit(_pe, format('Unknown symbol "%s" in output', [op[i]]));
        end;
    end;

    if not ouf.seekEof then 
      quit(_pe, 'Extra information in the output file');

    if (length(ip) <> length(op)) then begin
        quit(_wa, format('Program has length %d instead of %d', [length(op), length(ip)]));
    end;

    s := ip;
    p := length(s);
    rec(id);
    ni := o;

    s := op;
    p := length(s);
    rec(od);
    no := o;
    if p <> 0 then 
      quit(_wa, 'Wrong program -- extra stuff in stack');

    if oe <> od then begin
        quit(_wa, format('Promised %d elements, needed %d', [oe, od]));
    end;

    if (ni <> no) then begin
        quit(_wa, 'Programs do not match');
    end;

    ac := ans.readinteger();
    if od > ac then begin
        quit(_wa, format('Used %d elements instead of %d', [od, ac]));
    end;
    if od < ac then begin
        quit(_fail, format('Better then jury: Used %d elements instead of %d', [od, ac]));
    end;

    quit(_ok, format('%d elements', [ac]));
end.

(*
    Checker for NEERC'2004 Problem I: Irrelevant Elements
    (C) Georgy Korneev, Andrew Lopatin
*)

{$O-,Q+,R+}
{$APPTYPE CONSOLE}
uses
    testlib, SysUtils;

const 
    MaxToRead = 100000;

var
    jc, pc, jn: longint;
    i: longint;
    p:array [1..MaxToRead] of longint;

begin
    pc := ouf.readLongint();
    jc := ans.readLongint();

    if pc > MaxToRead then begin
        for i := 1 to pc do 
            ouf.readlongint; //to /dev/null, expect PE
        if not ouf.seekeof then 
            quit (_PE, 'End-of-file expected');
        quit (_WA, 'Too many numbers: ' + inttostr (pc));
    end;

    if pc < 0 then 
        quit (_PE, 'Invalid count: ' + inttostr (pc)); 
    //otherwise it may lead to PE after checking end-of-file

    for i := 1 to pc do 
        p[i] := ouf.readlongint;

    if not ouf.seekeof then 
        quit (_PE, 'End-of-file expected');

    if (jc <> pc) then begin
        quit(_wa, format('%d numbers instead of %d', [pc, jc]));
    end;

    for i := 1 to jc do begin
        jn := ans.readLongint();
        if p[i] <> jn then begin
            quit(_wa, format('%d number: %d instead of %d', [i, p[i], jn]));
        end;
    end;

    quit(_ok, format('%d numbers', [jc]));
end.

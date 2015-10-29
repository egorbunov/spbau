(*
    Checker for NEERC'2004 Problem H: Heapsort
    (C) Andrew Stankevich, Roman Elizarov
*)

program check_heapsort;

uses
    testlib, sysutils;

const
    maxn = 100000;

type
    tarr = array [1..maxn] of longint;

var
    pa, ja: tarr;
    jc, pc: longint;
    n: longint;

function count(b: tarr): longint;
var
    i, j, k, t: longint;
    r: longint;
begin
    r := 0;
    for i := n downto 1 do begin
        t := b[1]; b[1] := b[i]; b[i] := t;
        j := 1;
        while 2 * j <= i - 1 do begin
            k := j;
            if b[2 * j] > b[k] then k := 2 * j;
            if (2 * j + 1 <= i - 1) and (b[2 * j + 1] > b[k]) then k := 2 * j + 1;
            if j = k then break;
            inc(r);
            t := b[j]; b[j] := b[k]; b[k] := t;
            j := k;
        end; 
    end;
    count := r;
end;

var
    i: longint;
    u: array [1..maxn] of boolean;

begin
    n := inf.readlongint;

    for i := 1 to n do begin
        ja[i] := ans.readlongint;
    end;

    jc := count(ja);

    // Read participant's answer and check all PEs
    // We purposefully do not check line-by-line structure of the output here.

    for i := 1 to n do
        pa[i] := ouf.readlongint;

    if not ouf.seekEof then 
      quit(_pe, 'Extra information in the output file');

    // Check participant's answer for WAs

    for i := 1 to n do begin
        if (pa[i] < 1) or (pa[i] > n) then
            quit(_wa, format('invalid number: %d', [pa[i]]));
        if u[pa[i]] then
            quit(_wa, format('duplicate number: %d', [pa[i]]));
        u[pa[i]] := true;
        if (i > 1) and (pa[i] > pa[i div 2]) then
            quit(_wa, 'not heap');
    end;

    pc := count(pa);

    if (jc < pc) then
        quit(_fail, format('expected: %d, found: %d', [jc, pc]));
    if (jc > pc) then
        quit(_wa, format('expected: %d, found: %d', [jc, pc]));

    quit(_ok, format('%d operations', [jc]));
end.

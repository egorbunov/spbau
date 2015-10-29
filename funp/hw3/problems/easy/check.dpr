(*
    Checker for NEERC'2004 Problem E: Easy Trading
    (C) Georgy Korneev, Roman Elizarov
*)

{$q+,r+,o-}
uses
    testlib, sysutils;

(*
   Takes word from the same line of the output/answer stream.
   If 'ans' stream is over then returns empty string,
*)
function getWord(var stm: InStream): string;
begin
    if stm.seekEoln then begin
        if @stm = @ans then begin
            result := '';
            exit;
        end;
        stm.quit(_pe, 'Expected one more word on line');
    end;
    result := stm.readWord(blanks, blanks);
end;

var
    i: longint;
    jw, pw: string;
    jd, pd: longint;
    wa: String;
begin
    i := 0;
    wa := '';
    while not ouf.seekEof() do begin
        inc(i);
        if (wa = '') and ans.seekEof() then begin
            wa := format('Too many entries: %d', [i]);
        end;

        pw := getWord(ouf);
        jw := getWord(ans);

        if (pw <> 'SELL') and (pw <> 'BUY') then begin
            quit(_pe, format('Entry %d: "SELL" or "BUY" expected instead of "%s"', [i, pw]));
        end;

        if (wa = '') and (pw <> jw) then begin
            wa := format('Entry %d: "%s" expected instead of "%s"', [i, jw, pw]);
        end;

        pw := getWord(ouf);
        jw := getWord(ans);

        if (pw <> 'ON') then begin
            quit(_pe, format('Entry %d: "ON" expected instead of "%s"', [i, pw]));
        end;

        pw := getWord(ouf);
        jw := getWord(ans);

        if (pw <> 'DAY') then begin
            quit(_pe, format('Entry %d: "DAY" expected instead of "%s"', [i, pw]));
        end;

        if ouf.seekEoln then
            quit(_pe, 'Extpected number of the same line');
        pd := ouf.readLongint();
        if not ans.seekeof then begin
            jd := ans.readLongint();
            if (wa = '') and (pd <> jd) then begin
                wa := format('Entry %d: Day %d expected instead of day %d', [i, jd, pd]);
            end;
        end;

        if not ouf.seekEoln then
            quit(_pe, 'Extra data on line');

        ouf.nextLine();
        ans.nextLine();
    end;

    if wa <> '' then
        quit(_wa, wa);

    if not ans.seekEof then
        quit(_wa, 'Not enough entries in output');

    quit(_ok, format('%d entries', [i]));
end.

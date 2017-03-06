{$q+,r+,o-}
unit tools;


interface

const
    max_l = 10000;

var
    prog: string;

procedure shuffle();
procedure output();
function randomCh(l: longint): string;
function randomCh12(l: longint): string;
function randomCh23(l: longint): string;
function ring(l: longint): string;

implementation

uses
    sysutils, math;

var
    p: longint;
    o: string;


procedure doShuffle();
var
    i, j, l: longint;
    q: array[1..9] of string;
begin
    case prog[p] of
        'a': begin
            dec(p);
            o := 'a';
        end;
        '1'..'9': begin
            l := ord(prog[p]) - ord('0');
            dec(p);
            for i := 1 to l do begin
                doShuffle();
                q[i] := o;
            end;

            j := random(l) + 1;
            o := '';
            for i := 1 to l do begin
                o := q[(i + j) mod l + 1] + o;
            end;
            o := o + chr(l + ord('0'));
        end;
        else begin
            assert(false, format('Illegal symbol in program "%c"', [prog[p]]));
        end;
    end;
end;

procedure shuffle();
begin
    p := length(prog);
    doShuffle();
    prog := o;
end;

procedure output();
begin
    write(prog);
end;

function randomCh(l: longint): string;
var
    i, r, s: longint;
begin
    if l = 1 then begin
        result := 'a';
    end else begin
        r := random(min((l - 1), 9)) + 1;
        s := (l - 1) div r;
        result := '';
        for i := 1 to r - 1 do begin
            result := result + randomCh(s);
        end;
        assert(length(result) = s * (r - 1));
        result := result + randomCh(l - 1 - s * (r - 1)) + chr(r + ord('0'));
        assert(length(result) = l);
    end;
end;

function randomCh12(l: longint): string;
var
    i, r, s: longint;
begin
    if l = 1 then begin
        result := 'a';
    end else if l <= 4 then begin
        result := ring(l - 1);
    end else begin
        r := random(2) + 1;
        s := (l - 1) div r;
        result := '';
        for i := 1 to r - 1 do begin
            result := result + randomCh12(s);
        end;
        assert(length(result) = s * (r - 1));
        result := result + randomCh12(l - 1 - s * (r - 1)) + chr(r + ord('0'));
        assert(length(result) = l);
    end;
end;

function randomCh23(l: longint): string;
var
    i, r, s: longint;
begin
    if l = 1 then begin
        result := 'a';
    end else if l <= 4 then begin
        result := ring(l - 1);
    end else begin
        r := random(2) + 2;
        s := (l - 1) div r;
        result := '';
        for i := 1 to r - 1 do begin
            result := result + randomCh23(s);
        end;
        assert(length(result) = s * (r - 1));
        result := result + randomCh23(l - 1 - s * (r - 1)) + chr(r + ord('0'));
        assert(length(result) = l);
    end;
end;

function ring(l: longint): string;
var
    i: longint;
begin
    result := '';
    for i := 1 to l do begin
        result := result + 'a';
    end;
    result := result + chr(l + ord('0'));
end;

begin
end.

{$MINSTACKSIZE $001000000}
{$MAXSTACKSIZE $001000000}
{$q-,r-,o+}
uses
    sysutils;

const
    max_l = 100000;

var
    s, o: string;
    p: longint;

function rec: longint;
var
    i, j, l: longint;
    t: array [1..9] of longint;
    q: array [1..9] of string;
    shift, deep, mdeep: longint;
begin
    case s[p] of
        'a': begin
            dec(p);
            rec := 1;
            o := 'a';
        end;
        '1'..'9': begin
            l := ord(s[p]) - ord('0');
            dec(p);
            for i := 1 to l do begin
                t[i] := rec();
                q[i] := o;
            end;

            mdeep := maxlongint;
            shift := 0;
            for i := 1 to l do begin
                deep := 0;
                for j := 1 to l do begin
                    if deep < t[(i + j) mod l + 1] + l - j then begin
                        deep := t[(i + j) mod l + 1] + l - j;
                    end;
                end;
                if mdeep > deep then begin
                    mdeep := deep;
                    shift := i;
                end;
            end;

            o := '';
            for j := 1 to l do begin
                o := o + q[(shift + j) mod l + 1];
            end;
            rec := mdeep;
            o := o + chr(l + ord('0'));
        end;
        else begin
            assert(false, format('Illegal symbol in program "%c"', [s[p]]));
            rec := 0;
        end;
    end;
end;

begin
    assign(input, 'chandelier.in'); reset(input);
    assign(output, 'chandelier.out'); rewrite(output);

    readln(s);
    assert((0 <= length(s)) and (length(s) <= max_l), 'Length out of bounds');

    p := length(s);
    writeln(rec());
    writeln(o);
    assert(p = 0, 'Program too long');

    close(input);
    close(output);
end.
uses
    sysutils;
const
    max_l = 10000;
var
    s, o: string;
    stack: array[1..max_l] of string;
    level: array[1..max_l] of longint;
    c: longint;
    i, l, j: longint;
    shift, mshift, deep, mdeep: longint;
begin
    assign(input, 'chandelier.in'); reset(input);
    assign(output, 'chandelier.out'); rewrite(output);

    readln(s);
    assert((0 <= length(s)) and (length(s) <= max_l), 'Length out of bounds');

    c := 0;
    for i := 1 to length(s) do begin
        case s[i] of
            'a': begin
                inc(c);
                stack[c] := 'a';
                level[c] := 1;
            end;
            '1'..'9': begin
                l := ord(s[i]) - ord('0');
                mdeep := maxlongint;
                mshift := 0;
                for shift := 1 to l do begin
                    deep := 0;
                    for j := 1 to l do begin
                        if deep < level[c - (j + shift) mod l] + l - j then begin
                            deep := level[c - (j + shift) mod l] + l - j;
                        end;
                    end;
                    if mdeep > deep then begin
                        mdeep := deep;
                        mshift := shift;
                    end;
                end;

                o := '';
                for j := 1 to l do begin
                    o := stack[c - (j + mshift) mod l] + o;
                end;
                c := c - l + 1;
                assert(c > 0, 'Stack underflow');
                stack[c] := o + chr(l + ord('0'));
                level[c] := mdeep;
            end;
            else begin
                assert(false, format('Illegal symbol in program "%c"', [s[i]]));
            end;
        end;
    end;

    assert(c = 1, 'Program too long');

    writeln(level[1]);
    writeln(stack[1]);

    close(input);
    close(output);
end.
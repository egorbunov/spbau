{$O-,Q+,R+}
program document_indexing;

uses
    sysutils;

const
    maxl = 10000;
    maxs = 10001;
    maxp = 2501;

    alph: set of char = ['A'..'Z'];

var
    s: array [1..maxl] of string;
    page: array [1..maxl] of longint;
    i, l, n, m, p: longint;

    trie: array [1..maxs, char] of longint;
    use: array [1..maxs, 1..maxp] of boolean;
    final: array [1..maxs] of boolean;
    v: longint;

procedure insert(s: string; p: longint);
var
    r: longint;
    i: longint;
begin
    r := 1;
    for i := 1 to length(s) do begin
        if trie[r][s[i]] = 0 then begin
            inc(v);
            trie[r][s[i]] := v;
        end;
        r := trie[r][s[i]];
    end;
    final[r] := true;
    use[r][p] := true;
end;

procedure process(s: string; p: longint);
var
    i: longint;
    r: string;
begin
    r := '';
    s := uppercase(s);
    i := 1;
    while (i <= length(s)) and (not (s[i] in alph)) do begin
        inc(i);
    end;

    if i > length(s) then exit;

    while (i <= length(s)) and (s[i] in alph) do begin
        r := r + s[i];
        inc(i);
    end;

    insert(r, p);

    process(copy(s, i, length(s) - i + 1), p);
end;

procedure traverse(r: longint; s: string);
var
    c: char;
    i, state: longint;
begin
    if final[r] then begin
        write(s, ' ');
        state := 0;

        for i := 1 to p + 1 do begin
            if use[r][i] then begin
                if state = 0 then begin
                    write(i);
                    state := 2;
                end else if state = 1 then begin
                    write(',', i);
                    state := 2;
                end else if state = 2 then begin
                    state := 3;
                end else if state = 3 then begin
                    state := 4;
                end;
            end else begin
                if state = 2 then begin
                    state := 1;
                end else if state = 3 then begin
                    write(',', i - 1);
                    state := 1;
                end else if state = 4 then begin
                    write('-', i - 1);
                    state := 1;
                end;
            end;
        end;
        writeln;
    end;
    for c := 'A' to 'Z' do
        if trie[r][c] <> 0 then
            traverse(trie[r][c], s + c);
end;

begin
    reset(input, 'document.in');
    rewrite(output, 'document.out');

    readln(n);

    m := 0;
    while not eof do begin
        inc(m);
        readln(s[m]);
    end;
    inc(m);
    s[m] := '';

    p := 1;
    l := 0;
    for i := 1 to m do begin
        inc(l);
        page[i] := p;
        if s[i] = '' then begin
            if l = 1 then 
                dec(l)
            else if l = 2 then begin
                if (i > 2) and (s[i - 2] <> '') then begin
                    page[i - 2] := p;
                    inc(l);
                    if (i > 4) and (s[i - 4] = '') then begin
                        page[i - 3] := p;
                        inc(l);
                    end;
                end;
            end;
        end;
        if l = n then begin
            inc(p);
            l := 0;
            if s[i - 1] = '' then begin
                if (i < m) and (s[i + 1] <> '') then begin
                    page[i] := p;
                    inc(l);
                end;
            end;
        end;
    end;

    v := 1;
    for i := 1 to m do begin    
//        writeln(page[i], '  "', s[i], '"');
        process(s[i], page[i]);
    end;

    traverse(1, '');

    close(input);
    close(output);
end.
    
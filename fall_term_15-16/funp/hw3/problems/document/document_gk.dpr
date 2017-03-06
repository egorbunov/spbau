{$o-,r+,q+}
uses
    sysutils;

const
    max_n = 100;
    max_p = 100000;

type
    tword = record
        word: string;
        page: longint;
    end;

function remove(s: string): string;
var
    i: longint;
begin
    i := 1;
    while (i <= length(s)) and not (s[i] in ['A'..'Z']) do inc(i);
    remove := copy(s, i, length(s));
end;

function split(var s: string): string;
var
    i: longint;
begin
    i := 1;
    while (i <= length(s)) and (s[i] in ['A'..'Z']) do inc(i);
    if i <> 0 then begin
        split := copy(s, 1, i - 1);
        while (i <= length(s)) and (s[i] = ' ') do inc(i);
        s := remove(copy(s, i, length(s)));
    end else begin
        s := '';
        split := s;
    end;
end;

var
    i: longint;
    n, m: longint;
    txt: array [0..max_p + 1] of string;
    pages: array [1..max_p + 1] of longint;
    words: array [0..max_p + 1] of tword;
    t: tword;
    cw: longint;
    line: longint;
    page: longint;
    p: longint;
    pp: longint;
    state: longint;
    next: boolean;
    same: boolean;
    word: boolean;
    first: boolean;

procedure comma();
begin
    if first then begin
        first := false;
    end else begin
        write(',');
    end;
end;

function less(var w1, w2: tword): boolean;
begin
    less := (w1.word < w2.word) or (w1.word = w2.word) and (w1.page < w2.page);
end;

procedure qsort(l, r: longint);
var
    i, j: longint;
    w: tword;
begin
    i := l;
    j := r;
    w := words[(i * 13 + j * 17) div 30];
    while i <= j do begin
        while less(words[i], w) do inc(i);
        while less(w, words[j]) do dec(j);
        if i <= j then begin
            t := words[i]; words[i] := words[j]; words[j] := t;
            inc(i);
            dec(j);
        end;
    end;
    if l < j then qsort(l, j);
    if i < r then qsort(i, r);
end;

begin
    assign(input, 'document.in'); reset(input);
    assign(output, 'document.out'); rewrite(output);

    readln(n);

    m := 0;
    while not eof do begin
        inc(m);
        readln(txt[m]);
        txt[m] := upperCase(txt[m]);
    end;
    inc(m);
    if txt[m] <> '' then begin
        txt[m + 1] := '';
        txt[0] := '';
    end;

    line := 0;
    page := 1;
    p := 0;
    for i := 1 to m do begin
        inc(line);
        if txt[i] <> '' then begin
            if (line = n + 1) then begin
                inc(page);
                line := 1;
                p := p - n;
            end;
            pages[i] := page;
        end else begin
            if (line >= n) or (line = n - 1) and (txt[i + 2] <> '') then begin
                pages[i] := page;
                inc(page);
                line := 0;
            end;
            if (line = 2) and (p <> 0) then begin
                if (p <> -2) then begin
                    inc(pages[i - 2]);
                    inc(line);
                end else begin
                    inc(pages[i - 2]);
                    inc(pages[i - 3]);
                    line := line + 2;
                end;
                if (line >= n) or (line = n - 1) and (txt[i + 2] <> '') then begin
                    inc(page);
                    line := 0;
                end;
            end;
            p := line;
        end;
    end;

    cw := 0;
    for i := 1 to m do begin
        //writeln(pages[i], '  "', txt[i], '"');
        txt[i] := remove(txt[i]);
        while txt[i] <> '' do begin
            inc(cw);
            words[cw].word := split(txt[i]);
            words[cw].page := pages[i];
        end;
    end;

    qsort(1, cw);
    {
    for i := 1 to cw do begin
        for j := i + 1 to cw do begin
            if (words[i].word > words[j].word) or (words[i].word = words[j].word) and (words[i].page > words[j].page) then begin
            end;
        end;
    end;
    }
    words[cw + 1].word := '';

    pp := 0;
    state := 0;
    for i := 1 to cw do begin
        word := words[i].word <> words[i + 1].word;
        same := not word and (words[i + 1].page = words[i].page);
        next := not word and (words[i + 1].page = words[i].page + 1);
        if state = 0 then begin
            write(words[i].word, ' ');
            state := 1;
            first := true;
        end;
        case state of
            1: begin
                if word then begin
                    comma();
                    writeln(words[i].page);
                    state := 0;
                end else if next then begin
                    state := 2;
                    pp := words[i].page;
                end else if not same then begin
                    state := 1;
                    comma();
                    write(words[i].page);
                end;
            end;
            2: begin
                if word then begin
                    state := 0;
                    comma();
                    writeln(pp, ',', words[i].page);
                end else if next then begin
                    state := 3;
                end else if not same then begin
                    state := 1;
                    comma();
                    write(pp, ',', words[i].page);
                end;
            end;
            3: begin
                if word then begin
                    state := 0;
                    comma();
                    writeln(pp, '-', words[i].page);
                end else if next then begin
                    state := 3;
                end else if not same then begin
                    state := 1;
                    comma();
                    write(pp, '-', words[i].page);
                end;
            end;
        end
    end;

    close(input);
    close(output);
end.
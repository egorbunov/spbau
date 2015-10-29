{$R+,Q+,C+,O-}
{$apptype CONSOLE}

uses
	SysUtils;

const
	IN_FILE = 'document.in';
	OUT_FILE = 'document.out';

	MAX_SIZE = 20000;
	MAX_LINES = 20000;
	MAX_LINE_LEN = 200;
	MAX_ITEMS = 100000;

    ALPHA = ['a'..'z', 'A'..'Z'];
	
type
	TItem = record
    	text : string;
        page : integer;
    end;

var
	n : integer;

	nLines : integer;
	lines : array[1..MAX_LINES] of string;
	pages : array[1..MAX_LINES] of integer;

	nItems : integer;
    items : array[1..MAX_ITEMS] of TItem;

procedure ReadInput;
var
	s : string;

begin
	reset(input, IN_FILE);
	assert(filesize(input) <= MAX_SIZE);
	assert(not eoln);
	read(n);
	assert(eoln);
	readln;
	nLines := 0;
	while not eof do begin
		readln(s);
		assert((s = '') or not (s[1] = ' ') or not (s[length(s)] = ' '));
		assert(length(s) <= MAX_LINE_LEN);
		inc(nLines);
        lines[nLines] := UpperCase(s);
	end;
	close(input);
end;

procedure Paginate;
var
	i, j, k : integer;
    curPage, linesLeft, paraSize : integer;

    procedure NextPage;
    begin
    	inc(curPage);
        linesLeft := n;
    end;

    procedure NextLine;
    begin
    	dec(linesLeft);
        if linesLeft = 0 then
        	NextPage;
    end;

begin
	fillchar(pages, sizeof(pages), $ff);

	i := 1;
    curPage := 1;
    linesLeft := n;
    while i <= nLines do begin
        { Rule 1 }
    	while (i <= nLines) and (lines[i] = '') do
        	inc(i);
        if i > nLines then
        	break;

        j := i;
        while (j < nLines) and (lines[j + 1] <> '') do
        	inc(j);

		paraSize := j - i + 1;

        { Rule 2 }
        if (linesLeft = 1) and (paraSize > 1) then
        	NextPage;

		{ Rule 4 }
        if (paraSize = 2) and (linesleft = 1) or
           (paraSize = 3) and (linesLeft = 2) then
        	NextPage;

        for k := i to j do begin
        	{ Rule 3 }
        	if (paraSize > 3) and (k = j - 1) and (linesLeft = 1) then
            	NextPage;
        	pages[k] := curPage;
            NextLine;
        end;

		{ Skip one more line if not at the begining of page }
        if linesLeft < n then
        	NextLine;

        i := j + 1;
    end;
end;

procedure AddItem(text : string; page : integer);
begin
	inc(nItems);
    items[nItems].text := text;
    items[nItems].page := page;
end;

function IsLess(const a, b : TItem) : boolean;
begin
	if a.text < b.text then
    	result := true
    else if a.text > b.text then
    	result := false
    else
    	result := a.page < b.page;
end;

procedure QSort(l, r : integer);
var
	i, j : integer;
    x, y : TItem;

begin
	x := items[l + random(r - l)];
    i := l;
    j := r;
    while i <= j do begin
    	while IsLess(items[i], x) do
        	inc(i);
        while IsLess(x, items[j]) do
        	dec(j);
        if i <= j then begin
        	y := items[i];
            items[i] := items[j];
            items[j] := y;
            inc(i);
            dec(j);
        end;
    end;
    if l < j then
    	QSort(l, j);
    if i < r then
    	QSort(i, r);
end;

procedure MakeIndex;
var
	i, p, t : integer;
    s : string;

begin
	nItems := 0;
    for i := 1 to nLines do begin
    	s := lines[i] + ' ';
    	p := 1;
        while p < length(s) do begin
        	while (p < length(s)) and not (s[p] in ALPHA) do
            	inc(p);
            if p >= length(s) then
            	break;
            t := p;
            while s[t] in ALPHA do
            	inc(t);
            AddItem(copy(s, p, t - p), pages[i]);
            p := t;
        end;
    end;
end;

procedure WriteOutput;
var
	i, j, k, q, s : integer;

begin
	rewrite(output, OUT_FILE);
	i := 1;
    while i <= nItems do begin
    	j := i;
        while (j < nItems) and (items[j + 1].text = items[i].text) do
        	inc(j);

        write(items[i].text, ' ');

        k := i;
        while k <= j do begin
        	q := k;
            while (q < j) and (items[q + 1].page <= items[q].page + 1) do
            	inc(q);
            if items[q].page - items[k].page >= 2 then begin
                if k > i then
                	write(',');
                write(items[k].page, '-', items[q].page);
            end else begin
            	for s := k to q do begin
                	if (s = i) or (items[s-1].page < items[s].page) then begin
	                	if s > i then
    	                	write(',');
                    	write(items[s].page);
                    end;
                end;
            end;
            k := q + 1;
        end;

        writeln;

        i := j + 1;
    end;
	close(output);
end;

begin
	ReadInput;
    Paginate;
    MakeIndex;
    QSort(1, nItems);
    WriteOutput;
end.

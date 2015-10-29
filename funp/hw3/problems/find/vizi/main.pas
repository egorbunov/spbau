{$R+,Q+,O-}
unit main;

interface

uses
    Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
    Dialogs, Math, Menus;

const
    MAX_N = 1000;
    HANDLE_SIZE = 10;
    FRACT = 10000;

type
    TMainForm = class(TForm)
    mainMenu: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    Openinput1: TMenuItem;
    Openoutput1: TMenuItem;
    OpenOutput2: TMenuItem;
    ClearOutput1: TMenuItem;
    openDialog: TOpenDialog;
    saveDialog: TSaveDialog;
    ClearInput1: TMenuItem;
    N2: TMenuItem;
    View1: TMenuItem;
    labelsMenuItem: TMenuItem;
    N3: TMenuItem;
    NextTest1: TMenuItem;
    PreviousTest1: TMenuItem;
      procedure FormPaint(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Openinput1Click(Sender: TObject);
    procedure OpenOutput2Click(Sender: TObject);
    procedure ClearOutput1Click(Sender: TObject);
    procedure Openoutput1Click(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure ClearInput1Click(Sender: TObject);
    procedure labelsMenuItemClick(Sender: TObject);
    procedure NextTest1Click(Sender: TObject);
    procedure PreviousTest1Click(Sender: TObject);
    private
      { Private declarations }
    public
      { Public declarations }
    end;

	TPolygon = record
		n : integer;
		p : array[1..MAX_N] of TPoint;
	end;

var
	MainForm: TMainForm;
    minx, miny, maxx, maxy : integer;
    pin, pout : TPolygon;

    infile, outfile : string;

    draggedVertex : integer = 0;

procedure ReadAll;

implementation

{$R *.dfm}

procedure UpdateTitle;
var
	text : string;

begin
	text := '';
    if infile <> '' then
    	text := text + ExtractFileName(infile)
    else
    	text := text + '(untitled)';
    text := text + '/';
    if outfile <> '' then
    	text := text + ExtractFileName(outfile)
    else
    	text := text + '(untitled)';
	text := text + ' - Find the Border';
	MainForm.Caption := text;
end;

procedure ClearScale;
begin
    minx := 0;
    maxx := 100 * FRACT;
    miny := 0;
    maxy := 100 * FRACT;
end;

procedure CalcScale;
var
	i : integer;

begin
	if pin.n = 0 then begin
    	ClearScale;
    end else begin
        minx := pin.p[1].x;
        maxx := pin.p[1].x;
        miny := pin.p[1].y;
        maxy := pin.p[1].y;

        for i := 1 to pin.n do begin
            minx := min(minx, pin.p[i].x);
            miny := min(miny, pin.p[i].y);
            maxx := max(maxx, pin.p[i].x);
            maxy := max(maxy, pin.p[i].y);
        end;

        for i := 1 to pout.n do begin
            minx := min(minx, pout.p[i].x);
            miny := min(miny, pout.p[i].y);
            maxx := max(maxx, pout.p[i].x);
            maxy := max(maxy, pout.p[i].y);
        end;
    end;
end;

procedure ReadPoly(var p : TPolygon);
var
	i : integer;
    x, y : real;

begin
    read(p.n);
    for i := 1 to p.n do begin
    	read(x, y);
        p.p[i].x := round(x * FRACT);
        p.p[i].y := round(y * FRACT);
    end;
end;

procedure WritePoly(const p : TPolygon);
var
	i : integer;

begin
	writeln(p.n);
    for i := 1 to p.n do begin
    	writeln(p.p[i].x div FRACT, ' ', p.p[i].y div FRACT);
    end;
end;

procedure ReadInput(fn : string);
begin
	reset(input, fn);
    ReadPoly(pin);
    close(input);
    CalcScale;
    infile := fn;
    UpdateTitle;
end;

procedure WriteInput(fn : string);
begin
	rewrite(output, fn);
    WritePoly(pin);
    close(output);
    outfile := fn;
    UpdateTitle;
end;

procedure ReadOutput(fn : string);
begin
	reset(input, fn);
    ReadPoly(pout);
    close(input);
    CalcScale;
    outfile := fn;
    UpdateTitle;
end;

procedure ClearOutput;
begin
	pout.n := 0;
    outfile := '';
    UpdateTitle;
end;

procedure ClearInput;
begin
    pin.n := 3;
    pin.p[1].x := 70 * FRACT;
    pin.p[1].y := 50 * FRACT;
    pin.p[2].x := 50 * FRACT;
    pin.p[2].y := 70 * FRACT;
    pin.p[3].x := 30 * FRACT;
    pin.p[3].y := 50 * FRACT;
    ClearScale;
    infile := '';
	UpdateTitle;
end;

procedure ReadAll;
begin
	if paramcount = 0 then begin
    	ClearInput;
        exit;
    end;

	if paramcount > 0 then
	    ReadInput(paramstr(1));
    if paramcount > 1 then
    	ReadOutput(paramstr(2));
end;

function mapx(x : integer) : integer;
begin
    result := 20 + (x - minx) * (MainForm.Width - 40) div (maxx - minx);
end;

function mapy(y : integer) : integer;
begin
    result := 10 + (maxy - y) * (MainForm.Height - 70) div (maxy - miny);
end;

function unmapx(x : integer) : integer;
begin
	result := round(minx + (x - 20) * (maxx - minx) / (MainForm.Width - 40));
    result := FRACT * (result div FRACT);
    if result < 0 then result := 0;
    if result > 100 * FRACT then result := 100;
end;

function unmapy(y : integer) : integer;
begin
	result := round(maxy - (y - 10) * (maxy - miny) / (MainForm.Height - 70));
    result := FRACT * (result div FRACT);
    if result < 0 then result := 0;
    if result > 100 * FRACT then result := 100;
end;

function FindVertex(const p : TPolygon; x, y : integer) : integer;
var
	i : integer;

begin
	for i := 1 to p.n do begin
    	if (abs(x - mapx(p.p[i].x)) < HANDLE_SIZE) and
           (abs(y - mapy(p.p[i].y)) < HANDLE_SIZE) then begin
	    	result := i;
    	    exit;
        end;
    end;
    result := 0;
end;

procedure SplitEdge(var p : TPolygon; i : integer);
var
	j : integer;
    x, y : integer;

begin
	x := (p.p[i].x + p.p[1 + i mod p.n].x) div 2;
	y := (p.p[i].y + p.p[1 + i mod p.n].y) div 2;

    for j := p.n downto i + 1 do
    	p.p[j + 1] := p.p[j];

    p.p[i+1].x := x;
    p.p[i+1].y := y;
    inc(p.n);
end;

procedure RemoveVertex(var p : TPolygon; i : integer);
var
	j : integer;
    x, y : integer;

begin
    for j := i + 1 to p.n do
    	p.p[j - 1] := p.p[j];
    dec(p.n);
end;

procedure TMainForm.FormPaint(Sender: TObject);
type
	TPointArray = array of TPoint;

var
	a : TPointArray;
    i : integer;

	function makearray(const p : TPolygon) : TPointArray;
    var
    	i : integer;

    begin
    	setlength(result, p.n);
        for i := 1 to p.n do begin
        	result[i-1].x := mapx(p.p[i].x);
            result[i-1].y := mapy(p.p[i].y);
        end;
    end;

begin
    Canvas.Pen.Style := psClear;
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := Color;
	Canvas.Rectangle(Canvas.ClipRect);

	if pout.n > 0 then begin
        Canvas.Pen.Width := 0;
        Canvas.Brush.Style := bsSolid;
        Canvas.Brush.Color := clLtGray;
        Canvas.Polygon(makearray(pout));
    end;

    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Width := 1;
    Canvas.Pen.Color := clBlue;
    Canvas.Brush.Style := bsClear;
    a := makearray(pin);
    Canvas.Polygon(a);

	if pout.n > 0 then begin
        Canvas.Pen.Width := 2;
        Canvas.Pen.Color := clBlack;
        Canvas.Brush.Style := bsClear;
        Canvas.Polygon(makearray(pout));
    end;

    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Width := 1;
    Canvas.Pen.Color := clBlue;
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := Color;
    for i := 0 to length(a) - 1 do
    	Canvas.Ellipse(
        	a[i].x - HANDLE_SIZE div 2, a[i].y - HANDLE_SIZE div 2,
	        a[i].x + HANDLE_SIZE div 2, a[i].y + HANDLE_SIZE div 2);

    if MainForm.labelsMenuItem.Checked then begin
        Canvas.Pen.Style := psSolid;
        Canvas.Pen.Width := 1;
        Canvas.Pen.Color := clBlack;
	    for i := 0 to length(a) - 1 do
        	Canvas.TextOut(a[i].x, a[i].y, inttostr(i + 1));
    end;
end;

procedure TMainForm.Exit1Click(Sender: TObject);
begin
	Close;
end;

procedure TMainForm.Openinput1Click(Sender: TObject);
begin
	if openDialog.Execute then begin
    	ReadInput(openDialog.FileName);
		Invalidate;
    end;
end;

procedure TMainForm.OpenOutput2Click(Sender: TObject);
begin
	if openDialog.Execute then begin
    	ReadOutput(openDialog.FileName);
        Invalidate;
    end;
end;

procedure TMainForm.ClearOutput1Click(Sender: TObject);
begin
	ClearOutput;
	Invalidate;
end;

procedure TMainForm.Openoutput1Click(Sender: TObject);
begin
	if saveDialog.Execute then begin
    	WriteInput(saveDialog.FileName);
    end;
end;

procedure TMainForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
	i : integer;
begin
	i := FindVertex(pin, x, y);
    if i <> 0 then begin
	    if button = mbLeft then
        	draggedVertex := i
        else if button = mbRight then begin
        	SplitEdge(pin, i);
            Invalidate;
        end else if (button = mbMiddle) and (pin.n > 3) then begin
        	RemoveVertex(pin, i);
            Invalidate;
        end;
    end;
end;

procedure TMainForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
	if draggedVertex <> 0 then begin
        draggedVertex := 0;
    end;
end;

procedure TMainForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
	if draggedVertex <> 0 then begin
    	pin.p[draggedVertex].x := unmapx(x);
    	pin.p[draggedVertex].y := unmapy(y);
        Invalidate;
    end;
end;

procedure TMainForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
	if key = '+' then begin
    	CalcScale;
        Invalidate;
    end else if key = '-' then begin
    	ClearScale;
        Invalidate;
    end;
end;

procedure TMainForm.ClearInput1Click(Sender: TObject);
begin
	ClearInput;
    Invalidate;
end;

procedure TMainForm.labelsMenuItemClick(Sender: TObject);
begin
    labelsMenuItem.Checked := not labelsMenuItem.Checked;
    Invalidate;
end;

function SwitchFileName(fn : string; delta : integer) : string;
var
	name, path, ext : string;
	i, id, code : integer;

begin
	name := ExtractFileName(fn);
    path := ExtractFilePath(fn);
    ext := ExtractFileExt(fn);

    i := pos('.', name);
    if i <> 0 then
    	name := copy(name, 1, i - 1);

    val(name, id, code);
    if code <> 0 then begin
    	result := '';
        exit;
    end;

    inc(id, delta);
    name := format('%.2d', [id]);
    result := path + name + ext;

    if not FileExists(result) then
    	result := '';
end;

function SwitchFiles(delta : integer) : boolean;
var
	i, o : string;
    
begin
	result := false;

	if infile <> '' then
		i := SwitchFileName(infile, delta)
    else
    	i := '';

    if outfile <> '' then
	    o := SwitchFileName(outfile, delta)
    else
    	o := '';

    if i <> '' then
    	ReadInput(i);
    if o <> '' then
    	ReadOutput(o);

    result := true;
end;

procedure TMainForm.NextTest1Click(Sender: TObject);
begin
	if SwitchFiles(1) then
    	Invalidate;
end;

procedure TMainForm.PreviousTest1Click(Sender: TObject);
begin
	if SwitchFiles(-1) then
    	Invalidate;
end;

end.

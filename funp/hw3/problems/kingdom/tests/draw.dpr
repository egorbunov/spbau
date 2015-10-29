uses
    sysutils, math, graph, types, graphics;

const
    spread = 100;
    points = 3;    
    eps = 1e-15;

var
    n: longint;

function x(i: longint): longint;
var
    cx, cy: longint;
    s: extended;
begin
    cx := form.clientWidth div 2;
    cy := form.clientHeight div 2;
    //s := min(cx, cy) / 1.2;
    s := cx / 1.1 - spread * (i mod points);
    x := cx + round(s * cos(i / n * 2 * pi));
end;

function y(i: longint): longint;
var
    cx, cy: longint;
    s: extended;
begin
    cx := form.clientWidth div 2;
    cy := form.clientHeight div 2;
    s := min(cx, cy) / 1.2;
    s := cy / 1.1 - spread * (i mod points);
    y := cy + round(s * sin(i / n * 2 * pi));
end;

procedure line(x1, y1, x2, y2: longint);
begin
    canvas.moveTo(x1, y1);
    canvas.lineTo(x2, y2);
end;

procedure circle(x, y, r: longint);
begin
    canvas.ellipse(x - r, y - r, x + r, y + r);
end;

var
    i, m: longint;
    a1, a2, b1, b2: longint;
    v1, v2: longint;
begin
    initGraph();

    assign(input, paramstr(1)); reset(input);
    read(n, m, a1, b1, a2, b2);

    canvas.pen.color := $7f7f7f;
    canvas.brush.color := $7f7f7f;
    for i := 1 to n do begin
        circle(x(i), y(i), 3);
    end;

    canvas.pen.color := $ffffff;
    for i := 1 to m do begin
        read(v1, v2);
        line(x(v1), y(v1), x(v2), y(v2));
    end;
    canvas.pen.color := $00ffff;
    canvas.brush.color := $ff0000;
    circle(x(a2), y(a2), 10);

    canvas.pen.color := $ff00ff;
    canvas.brush.color := $00ff00;
    circle(x(b2), y(b2), 10);

    canvas.pen.color := $ff00ff;
    canvas.brush.color := $00ff00;
    circle(x(b1), y(b1), 5);

    canvas.pen.color := $00ffff;
    canvas.brush.color := $ff0000;
    circle(x(a1), y(a1), 5);

    waitForKey();

    close(input);
end.
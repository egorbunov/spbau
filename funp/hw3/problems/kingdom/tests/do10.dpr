uses tools;
    
begin
    randseed := 5632610;

    n  := 6;
    a1 := 1;
    b1 := 2;
    a2 := 2;
    b2 := 1;

    addEdge(1, 2);
    addEdge(2, 3);
    addEdge(3, 4);
    addEdge(4, 5);
    addEdge(5, 6);
    addEdge(6, 1);
    addEdge(4, 6);

    findFarest();

    shuffle();
    output();
end.
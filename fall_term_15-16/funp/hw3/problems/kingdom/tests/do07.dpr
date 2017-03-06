uses tools;
	
begin
	randseed := 5632607;

	n  := 6;
	a1 := 1;
	b1 := 2;
	a2 := 4;
	b2 := 5;

	addEdge(1, 2);
	addEdge(2, 3);
	addEdge(3, 4);
	addEdge(4, 5);
	addEdge(5, 6);
	addEdge(6, 1);

	shuffle();
	output();
end.
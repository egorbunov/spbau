uses tools;
	
begin
	randseed := 5632605;

	n  := 4;
	a1 := 1;
	b1 := 2;
	a2 := 3;
	b2 := 4;

	addEdge(1, 2);
	addEdge(2, 3);
	addEdge(3, 4);
	addEdge(4, 1);

	shuffle();
	output();
end.
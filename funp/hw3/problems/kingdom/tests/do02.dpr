uses tools;
	
begin
	randseed := 5632603;

	n  := 3;
	a1 := 1;
	b1 := 2;
	a2 := 3;
	b2 := 1;

	addEdge(1, 2);
	addEdge(2, 3);
	addEdge(3, 1);

	shuffle();
	output();
end.
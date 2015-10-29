uses tools;
	
begin
	randseed := 5632609;

	n  := 6;
	a1 := 1;
	b1 := 2;
	a2 := 5;
	b2 := 4;

	addEdge(1, 2);
	addEdge(2, 3);
	addEdge(3, 4);
	addEdge(4, 5);
	addEdge(5, 6);
	addEdge(6, 1);
	addEdge(3, 6);

	shuffle();
	output();
end.
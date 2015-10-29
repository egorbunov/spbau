/*
    Solution for NEERC'2004 Problem K: Kingdom of Magic
    (C) Roman Elizarov
    Note: this solution attempts to check correctness of the input
*/

import java.io.*;
import java.util.*;

public class kingdom_re_check {
	static final int MAXN = 100;
	static final int MAXM = 1000;

	static class Edge {
		int a;
		int b;

		Edge(int a, int b) {
			this.a = a;
			this.b = b;
		}	

		boolean equals(Edge e) {
			return a == e.a && b == e.b;
		}
	}

	static void saveEdge(Edge[] e, boolean[][] g, int i, Edge f) {
		e[i] = f;
		if (g[f.a][f.b])                                        
			throw new IllegalArgumentException("Duplicate portals");
		g[f.a][f.b] = true;
	}

	public static void main(String[] args) throws Exception {
		// Read input
		BufferedReader in = new BufferedReader(new FileReader("kingdom.in"));
		StringTokenizer st = new StringTokenizer(in.readLine());
		int n = Integer.parseInt(st.nextToken());
		int m = Integer.parseInt(st.nextToken());
		Edge e1 = new Edge(Integer.parseInt(st.nextToken()), Integer.parseInt(st.nextToken()));
		Edge e2 = new Edge(Integer.parseInt(st.nextToken()), Integer.parseInt(st.nextToken()));
		if (st.hasMoreTokens())
			throw new IllegalArgumentException("Extra data on the first line");
		if (n < 3 || n > MAXN)
			throw new IllegalArgumentException("n is out of range");
		if (m < 2 || m > MAXM)
			throw new IllegalArgumentException("m is out of range");

		boolean[][] g = new boolean[n + 1][n + 1];
		Edge[] e = new Edge[2 * m];

		for (int i = 0; i < m; i++) {
			st = new StringTokenizer(in.readLine());
			int p1 = Integer.parseInt(st.nextToken());
			int p2 = Integer.parseInt(st.nextToken());
			if (st.hasMoreTokens())
				throw new IllegalArgumentException("Extra data on the line");
			if (p1 < 1 || p1 > n || p2 < 1 || p2 > n || p1 == p2)
				throw new IllegalArgumentException("Invalid portal specification");
			saveEdge(e, g, 2 * i, new Edge(p1, p2));
			saveEdge(e, g, 2 * i + 1, new Edge(p2, p1));
		}
		in.close();

		int i1 = -1;
		int i2 = -1;

		for (int i = 0; i < 2 * m; i++) {
			if (e[i].equals(e1))
				i1 = i;
			if (e[i].equals(e2))
				i2 = i;
		}

		if (i1 < 0 || i2 < 0 || i1 == i2)
			throw new IllegalArgumentException("Invalid end or start");

		// Solve with Dejkstra algorithm
		int[] dist = new int[2 * m];
		int[] back = new int[2 * m];
		int[] cnt = new int[2 * m];
		boolean[] done = new boolean[2 * m];

		for (int i = 0; i < 2 * m; i++)
			dist[i] = Integer.MAX_VALUE;
		dist[i2] = 0;
		back[i2] = -1;
		cnt[i2] = 1;

		while (!done[i1]) {
			int i = -1;
			int min = Integer.MAX_VALUE;
			for (int j = 0; j < 2 * m; j++)
				if (!done[j] && dist[j] < min) {
					min = dist[j];
					i = j;
				}
			if (i < 0)
				throw new IllegalArgumentException("No path");
			done[i] = true;
			for (int j = 0; j < 2 * m; j++)
				if (!done[j]) {
					int cost = 0;
					if (e[i].a == e[j].a && g[e[i].b][e[j].b] || 
							e[i].b == e[j].b && g[e[i].a][e[j].a]) 
						cost = 1;
					else if (g[e[i].a][e[j].a] && g[e[i].b][e[j].b] && 
							(e[i].a != e[j].b || e[i].b != e[j].a))
						cost = 2;
					if (cost > 0 && dist[i] + cost < dist[j]) {
						dist[j] = dist[i] + cost;
						back[j] = i;
						cnt[j] = cnt[i] + 1;
					}
				}
		}

		// Write answer
		PrintWriter out = new PrintWriter(new FileWriter("kingdom.out"));
		out.println(dist[i1] + " " + cnt[i1]);
		for (int i = i1; i >= 0; i = back[i])
			out.println(e[i].a + " " + e[i].b);
		out.close();

	}

}

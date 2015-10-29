/*
    Solution for NEERC'2004 Problem J: Joke with Turtles
    (C) Roman Elizarov
    Note: this solution attempts to check correctness of the input
*/

import java.io.*;
import java.util.*;

public class joke_re_check {
	static final int MAXN = 1000;

	static class Turtle implements Comparable {
		int i; // original index
		int a;
		int b;

		public int compareTo(Object other) {
			Turtle o = (Turtle)other;
			int i = a - o.a; 
			if (i != 0)
				return i;
			return b - o.b;
		}

	}

	public static void main(String[] args) throws Exception {
		// Read input
		BufferedReader in = new BufferedReader(new FileReader("joke.in"));
		int n = Integer.parseInt(in.readLine());
		if (n < 1 || n > MAXN)
			throw new IllegalArgumentException("N is out of range");
		Turtle[] t = new Turtle[n + 1];
		for (int i = 0; i < n; i++) {
		        StringTokenizer st = new StringTokenizer(in.readLine());
		        t[i] = new Turtle();
		        t[i].i = i;
		        t[i].a = Integer.parseInt(st.nextToken());
		        t[i].b = Integer.parseInt(st.nextToken());
		        if (st.hasMoreTokens())
		        	throw new IllegalArgumentException("Extra data on line");
			if (t[i].a < 0 || t[i].a > MAXN || t[i].b < 0 || t[i].b > MAXN)
				throw new IllegalArgumentException("a or b are out of range");
		}
		if (in.readLine() != null)
			throw new IllegalArgumentException("Extra data in file");
		in.close();

 		// This is barrier turtle -- it lies
		t[n] = new Turtle();
		t[n].i = n;
		t[n].a = n;

 		// Sort turtles by A, then B
		Arrays.sort(t);

		// Find index of first turtle for each A so that for each given A
		// turtle with this A have indices z[A] <= i < z[A+1]
		int z[] = new int[n + 1];
		for (int i = 0, a = 0; i <= n; i++) {
			while (a <= n && a <= t[i].a)
				z[a++] = i;
		}

		// Find max number of turtles telling truth from right to left
		int best[] = new int[n + 1];
		int best_j0[] = new int[n];
		int best_j1[] = new int[n];
		int best_next[] = new int[n];
		for (int a = n; --a >= 0;) {
			// Let us compute best[a] 
			best[a] = best[a + 1];
			best_next[a] = a + 1;
			for (int j = z[a]; j < z[a + 1];) {
				// Count turtles with the same b
				int j0 = j;
				int b = t[j].b;
				int cnt = 1;
				j++;
				while (t[j].a == a && t[j].b == b) {
					cnt++;
					j++;
				}
				// Now we have cnt such turtles
				// of them, only up to n-a-b can be telling truth
				cnt = Math.min(cnt, n - a - b);
				if (cnt > 0) {
					int cc = cnt + best[n - b];
					if (cc > best[a]) {
						best[a] = cc;
						best_j0[a] = j0;
						best_j1[a] = j0 + cnt;
						best_next[a] = n - b;
					}
				}
			}
		}

		// Determine which turtles are telling truth
		boolean tt[] = new boolean[n];
		for (int a = 0; a < n; a = best_next[a])
			for (int j = best_j0[a]; j < best_j1[a]; j++)
				tt[t[j].i] = true;

		// Print answer
		PrintWriter out = new PrintWriter(new FileWriter("joke.out"));
		int cnt = n - best[0]; // that is how many lying turtles we shall have
		out.print(cnt);
		for (int i = 0; i < n; i++)
			if (!tt[i]) {
				out.print(" " + (i + 1)); // turtles are numberd from 1.
				cnt--;
			}
		if (cnt != 0) 
			throw new InternalError("Oops -- something wrong in our algorithm.");
		out.println();
		out.close();
	}
}

/*
    Solution for NEERC'2004 Problem B: Box
    (C) Roman Elizarov
    Note: this solution attempts to check correctness of the input
*/

import java.io.*;
import java.util.*;

public class box_re_check {
	public static void main(String[] args) throws Exception {
		// Read input
		int[] w = new int[6];
		int[] h = new int[6];
		BufferedReader in = new BufferedReader(new FileReader("box.in"));
		for (int i = 0; i < 6; i++) {
			StringTokenizer st = new StringTokenizer(in.readLine());
			w[i] = Integer.parseInt(st.nextToken());
			h[i] = Integer.parseInt(st.nextToken());
			if (w[i] < 0 || w[i] > 10000 || h[i] < 0 || h[i] > 10000) 
				throw new IllegalArgumentException("w, h out of range");
			if (st.hasMoreTokens())
				throw new IllegalArgumentException("Extra data on line");
		}
		if (in.readLine() != null)
			throw new IllegalArgumentException("Extra lines in file");
		in.close();

		// Sort each pair
		for (int i = 0; i < 6; i++)
			if (w[i] > h[i]) {
				int t = w[i];
				w[i] = h[i];
				h[i] = t;
			}

		// Sort pairs between themselves
		for (int i = 0; i < 6; i++)
			for (int j = i + 1; j < 6; j++)
				if (w[i] > w[j] || w[i] == w[j] && h[i] > h[j]) {
					int t = w[i];
					w[i] = w[j];
					w[j] = t;
					t = h[i];
					h[i] = h[j];
					h[j] = t;
				}

		// Now check pattern
		boolean possible = true;

		// 1) pairs 1&2, 3&4, 5&6 must be the same
		for (int i = 0; i < 3; i++)
			possible &= w[2*i] == w[2*i + 1] && h[2*i] == h[2*i + 1];
		
		// 2) Pairs 1, 3, 5 must look like (a,b) (a, c), (b, c)
		possible &= w[0] == w[2]; // a
		possible &= h[0] == w[4]; // b
		possible &= h[2] == h[4]; // c

		PrintWriter out = new PrintWriter(new FileWriter("box.out"));
		out.println(possible ? "POSSIBLE" : "IMPOSSIBLE");
		out.close();
	}
}

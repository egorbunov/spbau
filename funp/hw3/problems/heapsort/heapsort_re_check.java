/*
    Solution for NEERC'2004 Problem H: Heapsort
    (C) Roman Elizarov
    Note: this solution attempts to check correctness of the input
*/

import java.io.*;
import java.util.*;

public class heapsort_re_check {
	static final int MAXN = 50000;

	public static void main(String[] args) throws Exception {
		// Read input
		BufferedReader in = new BufferedReader(new FileReader("heapsort.in"));
		int n = Integer.parseInt(in.readLine());
		if (n < 1 || n > MAXN)
			throw new IllegalArgumentException("n is out of range");
		if (in.readLine() != null)
			throw new IllegalArgumentException("Extra data in file");
		in.close();

		// Solve
		int h[] = new int[n];
		h[0] = 1;
		for (int i = 1; i < n; i++) {
			h[i - 1] = i + 1;
			h[i] = 	1;
			// sift up
			int j = i - 1;
			while (j > 0) {
				int k = (j - 1) / 2;
				int t = h[j];
				h[j] = h[k];
				h[k] = t;
				j = k;
			}
		}

		// Write answer
		PrintWriter out = new PrintWriter(new FileWriter("heapsort.out"));
		for (int i = 0; i < n; i++) {
			if (i > 0)
				out.print(' ');
			out.print(h[i]);
		}
		out.println();
		out.close();
				
	}
}

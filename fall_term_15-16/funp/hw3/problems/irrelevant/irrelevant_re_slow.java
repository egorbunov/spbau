/*
    Solution for NEERC'2004 Problem I: Irrelevant Elements
    (C) Roman Elizarov
    Note: this solution only works for small N and is slow.
*/

import java.io.*;
import java.util.*;

public class irrelevant_re_slow {
	public static void main(String[] args) throws Exception {
		// Read data
		BufferedReader in = new BufferedReader(new FileReader("irrelevant.in"));
		StringTokenizer st = new StringTokenizer(in.readLine());
		int n = Integer.parseInt(st.nextToken());
		int m = Integer.parseInt(st.nextToken());
		if (st.hasMoreTokens())
			throw new IllegalArgumentException("Extra data on line");
		if (in.readLine() != null)
			throw new IllegalArgumentException("Extra data in file");
		in.close();

		// Solve
		int c[][] = new int[n][];
		c[0] = new int[1];
		c[0][0] = 1;
		for (int i = 1; i < n; i++) {
			c[i] = new int[i + 1];
			c[i][0] = 1;
			c[i][i] = 1;
			for (int j = 1; j < i; j++)
				c[i][j] = (c[i - 1][j - 1] + c[i - 1][j]) % m;
		}

		int k = 0;
		for (int i = 0; i < n; i++)
			if (c[n - 1][i] == 0)
				k++;

		// Write output
		PrintWriter out = new PrintWriter(new FileWriter("irrelevant.out"));
		out.println(k);
		String sep = "";
		for (int i = 0; i < n; i++)
			if (c[n - 1][i] == 0) {
				out.print(sep);
				sep = " ";
				out.print(i + 1);
			}
		out.println();
		out.close();
	}
}

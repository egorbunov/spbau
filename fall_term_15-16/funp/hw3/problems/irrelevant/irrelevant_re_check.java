/*
    Solution for NEERC'2004 Problem I: Irrelevant Elements
    (C) Roman Elizarov
    Note: this solution attempts to check correctness of the input
*/

import java.io.*;
import java.util.*;

public class irrelevant_re_check {
	static final int MAXN = 100000;
	static final int MAXM = 1000000000;

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

		if (n < 1 || n > MAXN) 
			throw new IllegalArgumentException("n is out of range");
		if (m < 1 || m > MAXM)
			throw new IllegalArgumentException("m is out of range");

		// Factorize m

		int[] p = new int[32]; // primes
		int[] q = new int[32]; // power of each prime in m
		int k = 0; // number of primes

		for (int i = 2; i * i <= m; i++)
			if (m % i == 0) {
				p[k] = i;
				do {
					q[k]++;
					m /= i;
				} while (m % i == 0);
				k++;
			}
		if (m != 1) {
			p[k] = m;
			q[k]++;
			k++;
		}

		int[] c = new int[32]; // power of each prime in current binomial
		List a = new ArrayList(); // answer here

		for (int i = 1; i < n; i++) {
			// Computing C(n - 1, i) = C(n - 1, i - 1) * (n - i) / i
			int rf = n - i;
			int rd = i;
			int cgood = 0;
			for (int j = 0; j < k; j++) {
				while (rf % p[j] == 0) {
					c[j]++;
					rf /= p[j];
				}
				while (rd % p[j] == 0) {
					c[j]--;
					rd /= p[j];					
			        }
			        if (c[j] >= q[j])
			        	cgood++;
				if (c[j] < 0)
					throw new InternalError("Oops...");
			}
			if (cgood == k)
				a.add(new Integer(i + 1));
		}

		// Write output
		PrintWriter out = new PrintWriter(new FileWriter("irrelevant.out"));
		out.println(a.size());
		String sep = "";
		for (int i = 0; i < a.size(); i++) {
			out.print(sep);
			sep = " ";
			out.print(a.get(i));
		}
		out.println();
		out.close();
	}
}

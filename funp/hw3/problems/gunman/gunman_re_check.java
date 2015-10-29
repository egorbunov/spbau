/*
    Solution for NEERC'2004 Problem G: Gunman
    (C) Roman Elizarov
    Note: this solution attempts to check correctness of the input
*/

import java.io.*;
import java.text.*;
import java.util.*;

public class gunman_re_check {
	static final int MAXN = 100;
	static final int MAXC = 1000;
	static final double EPS = 1e-6;

	static int nextCoord(StringTokenizer st) {
		int c = Integer.parseInt(st.nextToken());
		if (c <= 0 || c >= MAXC)
			throw new IllegalArgumentException("Coordinate is out of range");
		return c;
	}

	static double[] check(int c1, int c2, int z1, int z2, int n, int[] a, int[] b, int[] z) {
		double[] res = new double[n + 1];
		for (int i = 0; i <= n; i++) {
			res[i] = c1 + (c2 - c1) * (double)(z[i] - z1) / (z2 - z1);
			if (res[i] < a[i] - EPS || res[i] > b[i] + EPS)
				return null;
		}	
		return res;
	}

	static double[] solve(int n, int[] a, int[] b, int[] z) {
		for (int i = 0; i <= n; i++)
			for (int j = i + 1; j <= n; j++) {
				// we need to check left-rigth only!
				double[] res = check(a[i], b[j], z[i], z[j], n, a, b, z);
				if (res != null)
					return res;

			}
		return null;
	}

	public static void main(String[] args) throws Exception {
		// Read input
		BufferedReader in = new BufferedReader(new FileReader("gunman.in"));
		int n = Integer.parseInt(in.readLine());
		if (n < 2 || n > MAXN)
			throw new IllegalArgumentException("n is out of range");
		int[] x1 = new int[n + 1];
		int[] y1 = new int[n + 1];
		int[] x2 = new int[n + 1];
		int[] y2 = new int[n + 1];
		int[] z = new int[n + 1];
		// Window 0 is a dummy window for shooter itself
		x1[0] = -MAXC;
		x2[0] = 2 * MAXC;
		for (int i = 1; i <= n; i++) {
			StringTokenizer st = new StringTokenizer(in.readLine());
			x1[i] = nextCoord(st);
			y1[i] = nextCoord(st);
			x2[i] = nextCoord(st);
			y2[i] = nextCoord(st);
			z[i] = nextCoord(st);
			if (st.hasMoreTokens())
				throw new IllegalArgumentException("Extra data on line");
			if (x1[i] >= x2[i] || y1[i] >= y2[i])
				throw new IllegalArgumentException("Wrong corners");
			if (z[i] <= z[i - 1])
				throw new IllegalArgumentException("Not ordered by z");
		}
		if (in.readLine() != null)
			throw new IllegalArgumentException("Extra data in file");
		in.close();

		// Solve 
		double[] xr = solve(n, x1, x2, z);
		double[] yr = solve(n, y1, y2, z);

		// Write answer
		PrintWriter out = new PrintWriter(new FileWriter("gunman.out"));
		if (xr == null || yr == null)
			out.println("UNSOLVABLE");
		else {
			out.println("SOLUTION");
			NumberFormat nf = NumberFormat.getInstance(Locale.US);
			nf.setMinimumFractionDigits(6);
			nf.setMaximumFractionDigits(6);
			nf.setGroupingUsed(false);
			out.println(nf.format(xr[0]));
			for (int i = 1; i <= n; i++)
				out.println(nf.format(xr[i]) + " " + nf.format(yr[i]) + " " + nf.format(z[i]));
		}
		out.close();
	}
}

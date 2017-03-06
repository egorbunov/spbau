/*
    Solution for NEERC'2004 Problem E: Easy Trading
    (C) Roman Elizarov
*/

import java.io.*;
import java.util.*;

public class easy_re {
	public static void main(String[] args) throws Exception {
		// Read input
		BufferedReader in = new BufferedReader(new FileReader("easy.in"));
		StringTokenizer st = new StringTokenizer(in.readLine());
		int m = Integer.parseInt(st.nextToken());
		int n = Integer.parseInt(st.nextToken());
		int k = Integer.parseInt(st.nextToken());

		double[] p = new double[k];
		for (int i = 0; i < k; i++)
			p[i] = Double.parseDouble(in.readLine());
		in.close();

		// Solve & write output
		PrintWriter out = new PrintWriter(new FileWriter("easy.out"));
		double sm = 0;
		double sn = 0;
		int sign = 0;
		for (int i = 0; i < k; i++) {
			if (i >= m)
				sm -= p[i - m];
			if (i >= n)
				sn -= p[i - n];
			sm += p[i];
			sn += p[i];
			int new_sign = i < n - 1 ? 0 : (sm / m - sn / n > 0 ? 1 : -1);
			if (new_sign != sign) {
				sign = new_sign;
				out.println((sign > 0 ? "BUY" : "SELL")+ " ON DAY " + (i + 1));
			}
		}
		out.close();
	}
}


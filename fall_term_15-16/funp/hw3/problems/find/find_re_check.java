/*
    Solution for NEERC'2004 Problem F: Find the Border
    (C) Roman Elizarov
    Note: this solution attempts to check correctness of the input
*/

import java.io.*;
import java.util.*;

public class find_re_check {
	static final int MAXN = 100;
	static final int MAXXY = 100;
	static final double EPS = 1e-8;

	static double det(double x11, double x12, double x21, double x22) {
		return x11 * x22 - x12 * x21;
	}

	static double vec(double x11, double x12, double x21, double x22) {
		return x11 * x21 + x12 * x22;
	}

	/**
	 * Retruns true is x0 is between x1 and x2.
	 */
	static boolean isBetween(double x0, double x1, double x2) {
		return x1 < x2 ? x0 > x1 - EPS && x0 < x2 + EPS :
				x0 > x2 - EPS && x0 < x1 + EPS;
	}

	/**
	 * Returns true if (x0,y0) lies on the edge (x1,y2)--(x2,y2) or coincides with one of points.
	 */
	static boolean isOnEdge(double x0, double y0, double x1, double y1, double x2, double y2) {
		return Math.abs(det(x0 - x1, y0 - y1, x2 - x1, y2 - y1)) < EPS &&
				isBetween(x0, x1, x2) && isBetween(y0, y1, y2);
	}

	/**
	 * Returns angle from 0 to 2*pi (exclusive) between (xc,yc)--(xp,yp) and (xc,yc)--(xq,yq).
	 * Returns +INF if vectors are zeros or colliniar and point into the same direction.
	 */
	static double angle(double xc, double yc, double xp, double yp, double xq, double yq) {
		double sin = det(xp - xc, yp - yc, xq - xc, yq - yc);
		double cos = vec(xp - xc, yp - yc, xq - xc, yq - yc);
		if (Math.abs(sin) < EPS && cos > -EPS)
			return Double.POSITIVE_INFINITY;
		double a = Math.atan2(sin, cos);
		return a < 0 ? 2 * Math.PI + a : a;
	}

	/**
	 * Finds rightmost exit from point (xc,yc) if previous point was (xp,yp).
	 */
	static int findRightmost(int n, int[] x, int[] y, double xc, double yc, double xp, double yp) {
		// We'll scan all edges where this point lies on
		int result = -1;
		double best = Double.POSITIVE_INFINITY;
		for (int i = 0; i < n; i++)
			if (isOnEdge(xc, yc, x[i], y[i], x[(i + 1) % n], y[(i + 1) % n])) {
				double a = angle(xc, yc, xp, yp, x[i], y[i]);
				if (a < best) {
					best = a;
					result = i;
				}
				a = angle(xc, yc, xp, yp, x[(i + 1) % n], y[(i + 1) % n]);
				if (a < best) {
					best = a;
					result = (i + 1) % n;
				}
			}	
		if (result < 0)
			throw new IllegalStateException("Oops... cannot find rightmost");
		return result;
	}

	static double[] reallocate(double[] a) {
		double[] anew = new double[a.length * 2];
		System.arraycopy(a, 0, anew, 0, a.length);
		return anew;		
	}

	public static void main(String[] args) throws Exception {
		// Read input
		BufferedReader in = new BufferedReader(new FileReader("find.in"));
		int n = Integer.parseInt(in.readLine());
		if (n < 3 || n > MAXN)
			throw new IllegalArgumentException("n is out of range");
		int[] x = new int[n];
		int[] y = new int[n];
		for (int i = 0; i < n; i++) {
			StringTokenizer st = new StringTokenizer(in.readLine());
			x[i] = Integer.parseInt(st.nextToken());
			y[i] = Integer.parseInt(st.nextToken());
			if (st.hasMoreTokens())
				throw new IllegalArgumentException("extra data on line");
		}
		if (in.readLine() != null)
			throw new IllegalArgumentException("Extra data in file");
		in.close();

		// Make sure all vertices are different
		for (int i = 0; i < n; i++)
			for (int j = 0; j < i; j++)
				if (x[i] == x[j] && y[i] == y[j])
					throw new IllegalArgumentException("Vertices are not distinct");

		// Make sure adjacent edges are not collinear
		for (int i = 0; i < n; i++)
			if (Math.abs(det(x[(i + 1) % n] - x[i], y[(i + 1) % n] - y[i],
					x[(i + 2) % n] - x[(i + 1) % n], y[(i + 2) % n] - y[(i + 1) % n])) < EPS)
				throw new IllegalArgumentException("Adjacent edges are collinear");

		// Makes sure no point lies on the edge between other points
		for (int i = 0; i < n; i++)
			for (int j = 0; j < n; j++)
				if (j != i && j != (i + 1) % n &&
						isOnEdge(x[j], y[j], x[i], y[i], x[(i + 1) % n], y[(i + 1) % n]))
					throw new IllegalArgumentException("Vertex lies on the edge between two other vertices");


		// Now we'll initially reserve at most n points for the answer
		double[] xb = new double[n];
		double[] yb = new double[n];

		// Select lower left point to start with
		xb[0] = x[0];
		yb[0] = y[0];
		for (int i = 1; i < n; i++)
			if (x[i] < xb[0] || x[i] == xb[0] && y[i] < yb[0]) {
				xb[0] = x[i];
				yb[0] = y[i];
			}
				
		// assume "previous point" is to the left
		double xp = xb[0] - 1;
		double yp = yb[0];
		int m = 0; // current point is xb[0],yb[0];
		while (true) {
			double xc = xb[m];
			double yc = yb[m];
			// Find where to look for next good point
			int i = findRightmost(n, x, y, xc, yc, xp, yp);
			// Find next intersection on (xc,yc)--(x[i],y[i]);
			double xq = x[i];
			double yq = y[i];
			for (int j = 0; j < n; j++) {
				double x1 = x[j];
				double y1 = y[j];
				double x2 = x[(j + 1) % n];
				double y2 = y[(j + 1) % n];
				// Now intersect (xc,yc)--(xp,yq) and (x1,y1)--(x2,y2)
				// Original equations:
				//   x = xc + tcq * (xq - xc) = x1 + t12 * (x2 - x1)
				//   y = yc + tcq * (yq - yc) = y1 + t12 * (y2 - y1)
				// Solving:
				//   tcp * (xq - xc) + t12 * (x1 - x2) = x1 - xc
				//   tcp * (yq - yc) + t12 * (y1 - y2) = y1 - yc
				
				double mdet = det(xq - xc, x1 - x2, yq - yc, y1 - y2);
				if (Math.abs(mdet) < EPS)
					continue; // no intersection here
				double tcp = det(x1 - xc, x1 - x2, y1 - yc, y1 - y2) / mdet;
				double t12 = det(xq - xc, x1 - xc, yq - yc, y1 - yc) / mdet;
				if (tcp < EPS || tcp > 1 - EPS || t12 < EPS || t12 > 1 - EPS)
					continue; // no intersection here
				// Now we have new intersection point
				xq = x1 + t12 * (x2 - x1);
				yq = y1 + t12 * (y2 - y1);
			}
			// (xq,yq) is our intersection point
			m++;
			if (Math.abs(xb[0] - xq) < EPS && Math.abs(yb[0] - yq) < EPS)
				break; // we are back to original point!!!
			// Write next point
			if (m >= xb.length) {
				xb = reallocate(xb);
				yb = reallocate(yb);
			}
			xb[m] = xq;
			yb[m] = yq;
			xp = xc;
			yp = yc;
		}

		// Write answer
		PrintWriter out = new PrintWriter(new FileWriter("find.out"));
		out.println(m);
		for (int i = 0; i < m; i++)
			out.println(xb[i] + " " + yb[i]);
		out.close();
	}
}
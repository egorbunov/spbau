/*
    Solution for NEERC'2004 Problem A: Ancient Cipher
    (C) Roman Elizarov
    Note: this solution attempts to check correctness of the input
*/

import java.io.*;
import java.util.*;

public class ancient_re_check {
	final static int MAXLEN = 100;

	static int[] getProfile(String s) {
		if (s.length() == 0 || s.length() > MAXLEN)
			throw new IllegalArgumentException();
		int[] letters = new int[26];
		for (int i = 0; i < s.length(); i++)
			letters[s.charAt(i) - 'A']++;
		int[] result = new int[s.length()];
		for (int i = 0; i < 26; i++)
			if (letters[i] > 0)
				result[letters[i] - 1]++;
		return result;
	}

	public static void main(String[] args) throws Exception {
		// Read input
		BufferedReader in = new BufferedReader(new FileReader("ancient.in"));
		int[] p1 = getProfile(in.readLine());
		int[] p2 = getProfile(in.readLine());
		if (p1.length != p2.length)
			throw new IllegalArgumentException("Messages of different lengths");
		if (in.readLine() != null)
			throw new IllegalArgumentException("Extra data in file");
		in.close();

		// Solve 
		boolean same = true;
		for (int i = 0; i < p1.length; i++)
			same &= p1[i] == p2[i];

		// Write answer
		PrintWriter out = new PrintWriter(new FileWriter("ancient.out"));
		out.println(same ? "YES" : "NO");
		out.close();
	}
}

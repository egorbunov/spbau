/*
    Solution for NEERC'2004 Problem C: Chandelier
    (C) Roman Elizarov
    Note: this solution attempts to check correctness of the input
*/

import java.io.*;
import java.util.*;

public class chandelier_re_check {
	public static void main(String[] args) throws Exception {
		// Read input
		BufferedReader in = new BufferedReader(new FileReader("chandelier.in"));
                String prg = in.readLine();
                if (prg.length() > 10000)
                	throw new IllegalArgumentException("Input program is too long");
                if (prg.length() < 2)
                	throw new  IllegalArgumentException("Input program is too short -- not a chandelier");
		if (in.readLine() != null)
			throw new IllegalArgumentException("Extra data in file");
                in.close();

		// Solve
                Item chandelier = parse(prg);

		// Write output
		PrintWriter out = new PrintWriter(new FileWriter("chandelier.out"));
		out.println(chandelier.depth);
		print(out, chandelier);
		out.println();
		out.close();
	}

	private static class Item {
		final List children = new ArrayList();
		int depth = 1;
		int offset;
		boolean expanded; // for printing

		private void solve() {
			depth = Integer.MAX_VALUE;
			int n = children.size();
			for (int cur_offset = 0; cur_offset < n; cur_offset++) {
				int cur_depth = 0;
				for (int i = 0; i < n; i++)
					cur_depth = Math.max(cur_depth, i + 
						((Item)children.get((i + cur_offset) % n)).depth);
				if (cur_depth < depth) {
					depth = cur_depth;
					offset = cur_offset;
				}
			}
		}
	}

	private static final Item PENDANT = new Item();

	private static Item parse(String prg) {
		List stack = new ArrayList();
		for (int i = 0, n = prg.length(); i < n; i++) {
			char cmd = prg.charAt(i);
			if (cmd == 'a') 
				stack.add(PENDANT);
			else if (cmd >= '1' && cmd <= '9') {
				Item item = new Item();
				List tops = stack.subList(stack.size() - (cmd - '0'), stack.size());
				item.children.addAll(tops);
				item.solve();
				tops.clear();
				stack.add(item);
			} else
				throw new IllegalArgumentException("Wrong command: " + cmd);
		}
		if (stack.size() != 1)
			throw new IllegalArgumentException("Should have a single node at the end");
		return (Item)stack.get(0);
	}

	private static void print(PrintWriter out, Item item) {
		List stack = new ArrayList();
		stack.add(item);
		while (!stack.isEmpty()) {
			item = (Item)stack.remove(stack.size() - 1);
			if (item == PENDANT) {
				out.print('a');
				continue;
			}
			int n = item.children.size();
			if (item.expanded)
  				out.print((char)('0' + n));
			else {
				stack.add(item);
				item.expanded = true;
				for (int i = n; --i >= 0;)
					stack.add(item.children.get((i + item.offset) % n));
			}
		}
	}
}
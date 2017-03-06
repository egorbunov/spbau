/*
    Solution for NEERC'2004 Problem L: Lattice Animals
    (C) Roman Elizarov
    Note: this solution attempts to check correctness of the input
*/

import java.io.*;
import java.util.*;

public class lattice_re_check {
	// Constants

	static final int MAXN = 10;

	static final int ST_FREE = 0;
	static final int ST_ACTIVE = 1;
	static final int ST_SKIPPED = 2;
	static final int ST_USED = 3;

	static final int SYM_ID = 0;
	static final int SYM_ROTATE90 = 1;
	static final int SYM_ROTATE180 = 2;
	static final int SYM_ROTATE90_FLIPX = 3;
	static final int SYM_FLIPX = 4;

	static final int SYM_COUNT = 5;

	// Cell structure

	static class Cell {
		int x;
		int y;
		int state;
		Cell next;
	}

	// Variables

	static int n;
	static int w;
	static int h;

	static Cell[] cells = new Cell[(2*MAXN + 1) * (2*MAXN + 1)];

	static int[] cnt = new int[SYM_COUNT];

	// Methods

	static Cell cell(int x, int y) {
		return cells[(x + MAXN) + (y + MAXN) * (2*MAXN + 1)];
	}

	static void init() {
		// Initialization of cells
		for (int i = 0; i < cells.length; i++)
			cells[i] = new Cell();
		for (int x = -MAXN; x <= MAXN; x++)
			for (int y = -MAXN; y <= MAXN; y++) {
				Cell c = cell(x, y);
				c.x = x;
				c.y = y;
				c.state = (x < 0 || x == 0 && y < 0) ? ST_SKIPPED : ST_FREE;
			}
		cell(0, 0).state = ST_ACTIVE;
		cell(0, 0).next = null;
	}

	static void count(Cell used, int maxx, int miny, int maxy) {
		cnt[SYM_ID]++;
		int inc;
		// Check rotational 90 symmetry
		if ((maxx + miny + maxy) % 2 == 0) {
			// potentially rotateable 90 degrees
			inc = 1;
			for (Cell c = used; c != null; c = c.next) {
				int x = c.y + (maxx - maxy - miny) / 2;
				int y = -c.x + (maxy + miny + maxx) / 2;
				if (cell(x, y).state != ST_USED) {
					inc = 0;
					break;
				}	
			}
			cnt[SYM_ROTATE90] += inc;
			inc = 1;
			for (Cell c = used; c != null; c = c.next) {
				int x = maxx - (c.y + (maxx - maxy - miny) / 2);
				int y = -c.x + (maxy + miny + maxx) / 2;
				if (cell(x, y).state != ST_USED) {
					inc = 0;
					break;
				}	
			}
			cnt[SYM_ROTATE90_FLIPX] += inc;
		}
		// Check rotate 180 symmetry
		inc = 1;
		for (Cell c = used; c != null; c = c.next) {
			int x = maxx - c.x;
			int y = miny + maxy - c.y;
			if (cell(x, y).state != ST_USED) {
				inc = 0;
				break;
			}	

		}
		cnt[SYM_ROTATE180] += inc;
		// Check flipx
		inc = 1;
		for (Cell c = used; c != null; c = c.next) {
			int x = maxx - c.x;
			if (cell(x, c.y).state != ST_USED) {
				inc = 0;
				break;
			}	
		}
		cnt[SYM_FLIPX] += inc;

		
		// Uncomment to log this animal
		/*
		System.out.println("-----------------------");
		for (int x = 0; x <= maxx; x++) {
			for (int y = miny; y <= maxy; y++)
				System.out.print(cell(x, y).state == ST_USED ? "#" : ".");
			System.out.println();
		}
		*/
	}

	static Cell activateFree(Cell c, Cell new_active) {
		if (c.state != ST_FREE)
			return new_active;
		c.state = ST_ACTIVE;
		c.next = new_active;
		return c;
	}

	static void traverse(int n_used, Cell active, Cell used, int maxx, int miny, int maxy) {
		if ((maxx >= w || maxy - miny >= h) && (maxx >= h || maxy - miny >= w))
			return; // dont not fit into w x h in any orientation
		if (n_used == n) {
			count(used, maxx, miny, maxy);
			return;
		}	

		Cell old_active = active;
		while (active != null) {
			// Use active cell
			Cell next_active = active.next;
			active.state = ST_USED;
			active.next = used;
		
			// Now activate all free cells around it
			Cell new_active = next_active;
			new_active = activateFree(cell(active.x - 1, active.y    ), new_active);
			new_active = activateFree(cell(active.x    , active.y + 1), new_active);
			new_active = activateFree(cell(active.x + 1, active.y    ), new_active);
			new_active = activateFree(cell(active.x    , active.y - 1), new_active);

			traverse(n_used + 1, new_active, active, Math.max(maxx, active.x), 
				Math.min(miny, active.y), Math.max(maxy, active.y));

			// Restore activated cells to free state
			while (new_active != next_active) {
				new_active.state = ST_FREE;
				new_active = new_active.next;
			}

			// Skip active cell and restore prev pointer
			active.state = ST_SKIPPED;
			active.next = next_active;
			active = next_active;
		}

		// Restore originally activated
		while (old_active != null) {
			old_active.state = ST_ACTIVE;
			old_active = old_active.next;
		}
	}

	// Main

	public static void main(String[] args) throws Exception {
		BufferedReader in = new BufferedReader(new FileReader("lattice.in"));
		StringTokenizer st = new StringTokenizer(in.readLine());
		n = Integer.parseInt(st.nextToken());
		w = Integer.parseInt(st.nextToken());
		h = Integer.parseInt(st.nextToken());
		if (st.hasMoreTokens())
			throw new IllegalArgumentException("Extra data on line");
		if (in.readLine() != null)
			throw new IllegalArgumentException("Extra data in file");
		in.close();	

		if (n < 1 || n > MAXN)
			throw new IllegalArgumentException("n is out of range");
		if (w < 1 || w > n || h < 1 || h > n)
			throw new IllegalArgumentException("w,h are out of range");

		init();			
		traverse(0, cell(0, 0), null, 0, 0, 0);

		// Uncomment to log by-symmetry breakdown
		/*
		for (int i = 0; i < cnt.length; i++) {
			System.out.println(i + ": " + cnt[i]);
		}
		*/

		// Now using Bernside Lemma
		int sumcnt = cnt[SYM_ID] + 2 * cnt[SYM_ROTATE90] + cnt[SYM_ROTATE180] +
			2 * cnt[SYM_ROTATE90_FLIPX] + 2 * cnt[SYM_FLIPX];

		if (sumcnt % 8 != 0)
			throw new IllegalStateException("FAIL: Cannot divide " + sumcnt);

		PrintWriter out = new PrintWriter(new FileWriter("lattice.out"));
		out.println(sumcnt / 8);
		out.close();
	}
}


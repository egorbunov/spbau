/*
    Solution for NEERC'2004 Problem D: Document Index
    (C) Roman Elizarov
    Note: this solution attempts to check correctness of the input
*/

import java.io.*;
import java.util.*;


public class document_re_check {
        static final int MAX_SIZE = 20000;
        static final int MAX_LINE = 200;
        static final int MIN_PAGE = 4;
        static final int MAX_PAGE = 100;

        static void addIndex(Map index, int pageNo, String line) {
        	StringBuffer wordBuf = new StringBuffer();
        	for (int i = 0; i <= line.length(); i++) {
			char c = i < line.length() ? Character.toUpperCase(line.charAt(i)) : ' ';
			if (c >= 'A' && c <= 'Z')
				wordBuf.append(c);
			else if (wordBuf.length() != 0) {
				String wordStr = wordBuf.toString();
				wordBuf.setLength(0);
				Set pages = (Set)index.get(wordStr);
				if (pages == null)
					index.put(wordStr, pages = new TreeSet());
				pages.add(new Integer(pageNo));
			}
        	}
        }
	
	public static void main(String[] args) throws Exception {
		Map/*<String,Set<Integer>>*/ index = new TreeMap();
		List/*<String>*/ para = new ArrayList();
		int pageNo = 1; // current page number
		int lineNo = 1; // current line number in page
		int fileLineNo = 1; // current line number in file
		
		// Read and paginate input
		BufferedReader in = new BufferedReader(new FileReader("document.in"));
		String line = in.readLine();
		int size = line.length() + 1;
		int n = Integer.parseInt(line);
		if (n < MIN_PAGE || n > MAX_PAGE)
			throw new IllegalArgumentException("Invalid number of lines per page");
		String curLine;
		do {
			fileLineNo++;
			curLine = in.readLine();
			size += curLine == null ? 1 : (curLine.length() + 1);
			if (size > MAX_SIZE)
				throw new IllegalArgumentException("File is too big on line " + fileLineNo);
			if (curLine != null && curLine.length() != 0) {
				// Not empty line -- check & add to paragraph
				if (curLine.charAt(0) <= ' ' || curLine.charAt(curLine.length() - 1) <= ' ')
					throw new IllegalArgumentException("Leading or trailing blanks on line " + fileLineNo);
				if (curLine.length() > MAX_LINE)
					throw new IllegalArgumentException("Line is too long on line " + fileLineNo);
				para.add(curLine);
				continue;
			}
       			// Empty line -- flush paragraph
       			if (para.isEmpty())
       				throw new IllegalArgumentException("Unexpected empty line on line " + fileLineNo);
			// Add empty line (but not at the beginning of the page)
			if (lineNo > 1) {
				lineNo++;
				if (lineNo > n) {
					pageNo++;
					lineNo = 1;
				}
			}
			// If the last line of the page is the first line of paragraph with more that one line...
			if (lineNo == n && para.size() > 1) {
				pageNo++;
				lineNo = 1;
			}
			// If the last line of the page is the next-to-last line of paragraph with 2-3 lines...
			if (lineNo + para.size() == n + 2 && (para.size() == 2 || para.size() == 3)) {
				pageNo++;
				lineNo = 1;
			}
			// Layout paragraph lines and mind widow lines during process
			for (int i = 0; i < para.size(); i++) {
				String paraLine = (String)para.get(i);
				// If next-to-last line is last on the page, then move it to the next page
				if (i > 0 && i == para.size() - 2 && lineNo == n) {
					pageNo++;
					lineNo = 1;
				}
				addIndex(index, pageNo, paraLine);
				// Uncomment for pagination log
				//System.out.println(pageNo + ":" + lineNo + " " + paraLine);
				lineNo++;
				if (lineNo > n) {
					pageNo++;
					lineNo = 1;
				}

			}
			// Clear this paragraph (done with it)
			para.clear();
		} while (curLine != null);
		in.close();

		// Write index to output
		PrintWriter out = new PrintWriter(new FileWriter("document.out"));
		for (Iterator wit = index.entrySet().iterator(); wit.hasNext();) {
			Map.Entry entry = (Map.Entry)wit.next();
			String word = (String)entry.getKey();
			Set pages = (Set)entry.getValue();
			out.print(word);
			int prevPage = -1;
			int rangeStart = -1;
			Iterator pit = pages.iterator();
			char sep = ' ';
			boolean hasNext;
			do {
				hasNext = pit.hasNext();
				pageNo = hasNext ? ((Integer)pit.next()).intValue() : -1;
       				if (pageNo != prevPage + 1) {
					// Output previous range and clear
					if (rangeStart >= 0) {
						out.print(sep);
						sep = ',';
						out.print(rangeStart);
						if (prevPage > rangeStart) {
							out.print(prevPage == rangeStart + 1 ? ',' : '-');
							out.print(prevPage);
						}
					}
					rangeStart = pageNo;
				}
				prevPage = pageNo;
			} while (hasNext);
			out.println();
		}
		out.close();
	}
}


#include <iostream>
#include <queue>
#include <map>

namespace {
  using namespace std;

  struct Node {
    char c;
    bool isLeaf;
    string code;
    Node *l;
    Node *r;
    Node(char c) : c(c), isLeaf(true), l(NULL), r(NULL), code("") {}
    Node(Node *l, Node *r) : c(-1), isLeaf(false), l(l), r(r), code("") {}
  };

  void add(Node *root, char c, string &s) {
    for (int i = 0; i < s.length(); ++i) {
      if (root->l == NULL)
        root->l = new Node(NULL, NULL);
      if (root->r == NULL)
        root->r = new Node(NULL, NULL);

      if (s[i] == '0') {
        root = root->l;
      } else {
        root = root->r;
      }
    }
    root->code = s;
    root->c = c;
    root->isLeaf = true;
  }


  void huff_decode(vector<char> chars, vector<string> codes, string s) {
    Node *root = new Node(NULL, NULL);

    for (int i = 0; i < chars.size(); ++i) {
      add(root, chars[i], codes[i]);
    }

    Node *cur = root;
    for (int i = 0; i < s.size(); ++i) {
      if (s[i] == '0') {
        cur = cur->l;
      } else {
        cur = cur->r;
      }
      if (cur->isLeaf) {
        cout << cur->c;
        cur = root;
      }
    }

  }

}

int main() {
  int k, l;
  cin >> k >> l;
  char c, tmp;
  string code;
  vector<char> chars;
  vector<string> codes;
  for (int i = 0; i < k; ++i) {
    cin >> c >> tmp;
    cin >> code;
    chars.push_back(c);
    codes.push_back(code);
  }
  string s;
  cin >> s;
  huff_decode(chars, codes, s);
}
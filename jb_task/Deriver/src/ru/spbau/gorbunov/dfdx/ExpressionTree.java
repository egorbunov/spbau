package ru.spbau.gorbunov.dfdx;

/**
 * Created by Egor Gorbunov on 11.02.2016.
 * email: egor-mailbox@ya.ru
 */

/**
 * Valid tree -- tree with only left child || tree with both children || tree with no children (leaf)
 */
public class ExpressionTree {
    public ExpressionTree(Token token, ExpressionTree left, ExpressionTree right) {
        this.token = token;
        this.left = left;
        this.right = right;
    }

    public ExpressionTree(Token token) {
        this(token, null, null);
    }

    /**
     * case of only one child
     */
    public ExpressionTree(Token token, ExpressionTree child) {
        this(token, child, null);
    }

    public String buildStr() {
        return null;
    }

    @Override
    public String toString() {
        if (left == null && right == null) {
            return token.getToken();
        } else {
            String str = "";
            if (left != null) {
                str += left.toString() + " ";
            }
            if (right != null) {
                str += right.toString() + " ";
            }
            str += token.getToken();
            return str;
        }
    }

    private Token token;
    private ExpressionTree left;
    private ExpressionTree right;
}

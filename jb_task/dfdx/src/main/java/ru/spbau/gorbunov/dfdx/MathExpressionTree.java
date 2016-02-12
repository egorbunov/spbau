package ru.spbau.gorbunov.dfdx;

/**
 * Created by Egor Gorbunov on 11.02.2016.
 * email: egor-mailbox@ya.ru
 */

/**
 * Valid expression tree -- tree with only left child || tree with both children || tree with no children (leaf)
 */
public class MathExpressionTree {
    public MathExpressionTree(Token token, MathExpressionTree left, MathExpressionTree right) {
        this.token = token;
        this.left = left;
        this.right = right;
    }

    public MathExpressionTree(Token token) {
        this(token, null, null);
    }

    /**
     * case of only one child
     */
    public MathExpressionTree(Token token, MathExpressionTree child) {
        this(token, child, null);
    }

    public String buildExpressionStr() {
        final String DELIMITER = " ";
        switch (token.getType()) {
            case BINARY_OP:
                return "(" + left.buildExpressionStr() + DELIMITER + token.getTokenStr() + DELIMITER + right.buildExpressionStr() + ")";
            case FUN:
                return token.getTokenStr() + "(" + left.buildExpressionStr() + ")";
            case VARIABLE:
            case NUMBER:
                return token.getTokenStr();
            default:
                throw new RuntimeException("Bad token node in expression tree: [ " + token.toString() + " ]!");
        }
    }

    public String buildRPNStr() {
        final String DELIMITER = " ";
        if (left == null && right == null) {
            return token.getTokenStr();
        } else {
            String str = "";
            if (left != null) {
                str += left.toString() + DELIMITER;
            }
            if (right != null) {
                str += right.toString() + DELIMITER;
            }
            str += token.getTokenStr();
            return str;
        }
    }

    public MathExpressionTree copy() {
        return new MathExpressionTree(
                token,
                left == null ? null : left.copy(),
                right == null ? null : right.copy()
        );
    }

    /**
     * RPM string
     */
    @Override
    public String toString() {
        return buildRPNStr();
    }

    public Token getToken() {
        return token;
    }

    public MathExpressionTree getLeft() {
        return left;
    }

    public MathExpressionTree getRight() {
        return right;
    }

    private Token token;

    public void setToken(Token token) {
        this.token = token;
    }

    public void setLeft(MathExpressionTree left) {
        this.left = left;
    }

    public void setRight(MathExpressionTree right) {
        this.right = right;
    }

    private MathExpressionTree left;
    private MathExpressionTree right;
}

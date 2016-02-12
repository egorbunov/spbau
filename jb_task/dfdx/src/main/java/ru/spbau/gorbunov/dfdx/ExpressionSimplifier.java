package ru.spbau.gorbunov.dfdx;

/**
 * Created by Egor Gorbunov on 12.02.16.
 * email: egor-mailbox@ya.ru
 */

/**
 * Extremely simple simplifier
 */
public class ExpressionSimplifier {

    public static void simplify(MathExpressionTree tree) {
        TokenTag tag = tree.getToken().getType();
        if (tag == TokenTag.VARIABLE || tag == TokenTag.NUMBER)
            return;

        if (tag == TokenTag.FUN) {
            simplify(tree.getLeft());
        } else if (tag == TokenTag.BINARY_OP) {
            simplify(tree.getLeft());
            simplify(tree.getRight());
            MathExpressionTree newTree = eval(
                    tree.getLeft(),
                    tree.getRight(),
                    BinaryOperator.fromString(tree.getToken().getTokenStr())
            );
            if (newTree != null) {
                tree.setLeft(newTree.getLeft());
                tree.setRight(newTree.getRight());
                tree.setToken(newTree.getToken());
            }
        }

    }

    private static MathExpressionTree eval(Double l, Double r, BinaryOperator bo) {
        Double res = null;
        switch (bo) {
            case DIV:
                res = l / r;
                break;
            case MUL:
                res = l * r;
                break;
            case SUB:
                res = l - r;
                break;
            case SUM:
                res = l + r;
                break;
        }
        String resStr = (Math.round(res) == res) ? Long.toString(Math.round(res)) : Double.toString(res);
        return new MathExpressionTree(new Token(TokenTag.NUMBER, resStr));
    }

    /**
     * Handling cases like [0 * x], [0 / x], [5+3], ...
     */
    private static MathExpressionTree eval(MathExpressionTree leftExpr, MathExpressionTree rightExpr, BinaryOperator bo) {
        Token left = leftExpr.getToken();
        Token right = rightExpr.getToken();
        Double rVal = null;
        Double lVal = null;
        if (right.getType() == TokenTag.NUMBER) {
            rVal = Double.valueOf(right.getTokenStr());
            if (rVal == 0.0 && bo == BinaryOperator.DIV) {
                throw new BadInputExpressionException("Division by zero.");
            }
            if (rVal == 0.0 && bo == BinaryOperator.MUL) {
                return rightExpr;
            }
            if (rVal == 0.0 && bo == BinaryOperator.SUM) {
                return leftExpr;
            }
            if (rVal == 1.0 && (bo == BinaryOperator.MUL || bo == BinaryOperator.DIV)) {
                return leftExpr;
            }
        }
        if (left.getType() == TokenTag.NUMBER) {
            lVal = Double.valueOf(left.getTokenStr());
            if (lVal == 0.0 && (bo == BinaryOperator.MUL || bo == BinaryOperator.DIV)) {
                return leftExpr;
            }
            if (lVal == 0.0 && bo == BinaryOperator.SUM) {
                return rightExpr;
            }
            if (lVal == 1.0 && bo == BinaryOperator.MUL) {
                return rightExpr;
            }
        }
        if (rVal != null && lVal != null) {
            return eval(lVal, rVal, bo);
        } else if (left.getType() == TokenTag.VARIABLE
                && right.getType() == TokenTag.VARIABLE
                && left.getTokenStr().equals(right.getTokenStr())) {
            if (bo == BinaryOperator.SUM) {
                return new MathExpressionTree(new Token(TokenTag.BINARY_OP, BinaryOperator.MUL.getString()),
                        new MathExpressionTree(new Token(TokenTag.NUMBER, "2")),
                        rightExpr);
            } else if (bo == BinaryOperator.SUB) {
                return new MathExpressionTree(new Token(TokenTag.NUMBER, "0"));
            } else if (bo == BinaryOperator.DIV) {
                return new MathExpressionTree(new Token(TokenTag.NUMBER, "1"));
            }
        }
        return null;
    }
}

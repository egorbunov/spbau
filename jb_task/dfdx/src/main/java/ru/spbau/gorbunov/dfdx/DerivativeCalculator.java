package ru.spbau.gorbunov.dfdx;

/**
 * Created by Egor Gorbunov on 11.02.2016.
 * email: egor-mailbox@ya.ru
 */
public class DerivativeCalculator {

    /**
     * Method builds expression tree for given math expression tree
     *
     * @param expr expression tree to differentiate
     * @param variable differential variable
     * @return expression tree for derivative
     */
    public static MathExpressionTree differentiate(MathExpressionTree expr, String variable) {
        Token token = expr.getToken();
        switch (token.getType()) {
            case BINARY_OP:
                BinaryOperator op = BinaryOperator.fromString(token.getTokenStr());
                MathExpressionTree lDer = differentiate(expr.getLeft(), variable);
                MathExpressionTree rDer = differentiate(expr.getRight(), variable);
                switch (op) {
                    case SUB:
                    case SUM:
                        /*
                            expression tree for: [ f' + g' ] or [ f' - g' ]
                         */
                        return new MathExpressionTree(expr.getToken(), lDer, rDer);
                    case DIV:
                        /*
                            creating tree for expression: (f/g)' = (f' * g - g' * f) / (g * g)
                            f -- left expression tree node
                            r -- right expression tree node
                         */
                        return new MathExpressionTree(
                                new Token(TokenTag.BINARY_OP, "/"),
                                new MathExpressionTree
                                        (
                                                new Token(TokenTag.BINARY_OP, BinaryOperator.SUB.getString()),
                                                new MathExpressionTree
                                                        (
                                                                new Token(TokenTag.BINARY_OP,
                                                                        BinaryOperator.MUL.getString()),
                                                                lDer,
                                                                expr.getRight().copy()
                                                        ),
                                                new MathExpressionTree
                                                        (
                                                                new Token(TokenTag.BINARY_OP,
                                                                        BinaryOperator.MUL.getString()),
                                                                rDer,
                                                                expr.getLeft().copy()
                                                        )
                                        ),
                                new MathExpressionTree
                                        (
                                                new Token(TokenTag.BINARY_OP, BinaryOperator.MUL.getString()),
                                                expr.getRight().copy(),
                                                expr.getRight().copy()));
                    case MUL:
                        /*
                            creating expression tree for: (f*g)' = f' g + g' * f
                         */
                        return new MathExpressionTree(
                                        new Token(TokenTag.BINARY_OP, BinaryOperator.SUM.getString()),
                                        new MathExpressionTree
                                                (
                                                        new Token(TokenTag.BINARY_OP, BinaryOperator.MUL.getString()),
                                                        lDer,
                                                        expr.getRight().copy()
                                                ),
                                        new MathExpressionTree
                                                (
                                                        new Token(TokenTag.BINARY_OP, BinaryOperator.MUL.getString()),
                                                        rDer,
                                                        expr.getLeft().copy()
                                                )
                        );
                }
            case NUMBER:
                // der. of constant is 0
                return new MathExpressionTree(new Token(TokenTag.NUMBER, "0"));
            case VARIABLE:
                if (expr.getToken().getTokenStr().equals(variable)) {
                    return new MathExpressionTree(new Token(TokenTag.NUMBER, "1"));
                } else {
                    return new MathExpressionTree(new Token(TokenTag.NUMBER, "0"));
                }
            case FUN:
                MathFunction fun = MathFunction.fromString(token.getTokenStr());
                // TODO: unify composition derivative
                switch (fun) {
                    case EXP:
                        return new MathExpressionTree(
                                new Token(TokenTag.BINARY_OP, BinaryOperator.MUL.getString()),
                                new MathExpressionTree(
                                        new Token(TokenTag.FUN, MathFunction.EXP.getStr()),
                                        expr.getLeft().copy()
                                ),
                                differentiate(expr.getLeft(), variable)
                        );
                    case LN:
                        return new MathExpressionTree(
                                new Token(TokenTag.BINARY_OP, BinaryOperator.MUL.getString()),
                                new MathExpressionTree(
                                        new Token(TokenTag.BINARY_OP, BinaryOperator.DIV.getString()),
                                        new MathExpressionTree(new Token(TokenTag.NUMBER, "1")),
                                        expr.getLeft().copy()
                                ),
                                differentiate(expr.getLeft(), variable)
                        );
                }
            default:
                throw new RuntimeException("Bad token node in expression tree!");
        }
    }
}

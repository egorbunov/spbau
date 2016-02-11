package ru.spbau.gorbunov.dfdx;

import java.util.Stack;

/**
 * Created by Egor Gorbunov on 11.02.2016.
 * email: egor-mailbox@ya.ru
 */
public class Parser {
    public static ExpressionTree parse(Token[] tokens) {
        Stack<ExpressionTree> evalStack = new Stack<>();
        Stack<Token> operationStack = new Stack<>();
        for (Token token : tokens) {
            TokenTag type = token.getType();
            switch (type) {
                case VARIABLE:
                case NUMBER:
                    evalStack.push(new ExpressionTree(token));
                    break;
                case FUN:
                case LEFT_PAREN:
                    operationStack.push(token);
                    break;
                case RIGHT_PAREN:
                    while (!operationStack.empty() && !operationStack.peek().getType().equals(TokenTag.LEFT_PAREN)) {
                        evalOperation(evalStack, operationStack);
                    }
                    if (operationStack.empty()) {
                        throw (new RuntimeException("Bad input expression!")); // TODO: switch to custom exception
                    }
                    operationStack.pop();
                    break;
                case BINARY_OP:
                    BinaryOperator op = BinaryOperator.fromString(token.getToken());
                    while (!operationStack.empty()) {
                        Token top = operationStack.peek();
                        boolean canBeEvaluated = top.getType().equals(TokenTag.FUN); // function must be evaluated
                        // binary operation can be evaluated if it has higher priority
                        if ((!canBeEvaluated) && top.getType().equals(TokenTag.BINARY_OP)) {
                            BinaryOperator topOp = BinaryOperator.fromString(top.getToken());
                            canBeEvaluated = op.isRightAssoc() ? (topOp.getPriority() > op.getPriority())
                                    : (topOp.getPriority() >= op.getPriority());
                        }
                        if (!canBeEvaluated) {
                            break;
                        }
                        evalOperation(evalStack, operationStack);
                    }
                    operationStack.push(token);
                    break;
            }
        }

        while (!operationStack.empty()) {
            evalOperation(evalStack, operationStack);
        }

        return evalStack.pop();
    }

    private static void evalOperation(Stack<ExpressionTree> evalStack, Stack<Token> operationStack) {
        if (operationStack.peek().getType().equals(TokenTag.BINARY_OP)) {
            ExpressionTree rp = evalStack.pop();
            ExpressionTree lp = evalStack.pop();
            evalStack.push(new ExpressionTree(operationStack.pop(), lp, rp));
        } else if (operationStack.peek().getType().equals(TokenTag.FUN)) {
            evalStack.push(new ExpressionTree(operationStack.pop(), evalStack.pop()));
        }
    }
}

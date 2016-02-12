package ru.spbau.gorbunov.dfdx;

import java.util.Stack;

/**
 * Created by Egor Gorbunov on 11.02.2016.
 * email: egor-mailbox@ya.ru
 */
public class Parser {
    public static MathExpressionTree parse(Token[] tokens) {
        Stack<MathExpressionTree> evalStack = new Stack<>();
        Stack<Token> operationStack = new Stack<>();
        for (Token token : tokens) {
            TokenTag type = token.getType();
            switch (type) {
                case VARIABLE:
                case NUMBER:
                    evalStack.push(new MathExpressionTree(token));
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
                        throw (new BadInputExpressionException("Bad input expression!"));
                    }
                    operationStack.pop();
                    break;
                case BINARY_OP:
                    BinaryOperator op = BinaryOperator.fromString(token.getTokenStr());
                    while (!operationStack.empty()) {
                        Token top = operationStack.peek();
                        boolean canBeEvaluated = top.getType().equals(TokenTag.FUN); // function must be evaluated
                        // binary operation can be evaluated if it has higher priority
                        if ((!canBeEvaluated) && top.getType().equals(TokenTag.BINARY_OP)) {
                            BinaryOperator topOp = BinaryOperator.fromString(top.getTokenStr());
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

        if (evalStack.size() != 1) {
            throw new BadInputExpressionException("wong input!");
        }

        return evalStack.pop();
    }

    private static void evalOperation(Stack<MathExpressionTree> evalStack, Stack<Token> operationStack) {
        if (operationStack.peek().getType().equals(TokenTag.BINARY_OP)) {
            if (evalStack.size() < 2) {
                throw new BadInputExpressionException("Bad operator usage!");
            }
            MathExpressionTree rp = evalStack.pop();
            MathExpressionTree lp = evalStack.pop();
            evalStack.push(new MathExpressionTree(operationStack.pop(), lp, rp));
        } else if (operationStack.peek().getType().equals(TokenTag.FUN)) {
            if (evalStack.size() < 1) {
                throw new BadInputExpressionException("Bad function usage!");
            }
            evalStack.push(new MathExpressionTree(operationStack.pop(), evalStack.pop()));
        } else if (operationStack.peek().getType().equals(TokenTag.LEFT_PAREN)) {
            throw new BadInputExpressionException("Bad parenthesis!");
        } else {
            throw new BadInputExpressionException("Bad input =(");
        }
    }
}

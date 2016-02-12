package ru.spbau.gorbunov.dfdx;


import java.util.ArrayList;

/**
 * Created by Egor Gorbunov on 11.02.2016.
 * email: egor-mailbox@ya.ru
 */
public class Lexer {

    public static Token[] tokenize(String expression) {
        ArrayList<Token> tokens = new ArrayList<>();

        State curState = State.START;
        String curToken = "";
        for (int i = 0; i <= expression.length(); i++) {
            Character c = null;
            if (i < expression.length()) {
                c = expression.charAt(i);
                if (Character.isSpaceChar(c)) {
                    continue;
                }
            }

            State prevState = curState;
            curState = go(prevState, c);

            if (curState.equals(State.UNKNOWN)) {
                throw new BadInputExpressionException("Unknown characters and bad input!");
            }
            if (curState.equals(State.ERROR)) {
                throw new BadInputExpressionException("Bad input expression!");
            }

            if (prevState.equals(State.PAREN) || (!prevState.equals(curState) && !curToken.isEmpty())) {
                switch (prevState) {
                    case OPERATOR:
                        tokens.add(new Token(TokenTag.BINARY_OP, curToken));
                        break;
                    case PAREN:
                        tokens.add(new Token(curToken.equals("(") ? TokenTag.LEFT_PAREN : TokenTag.RIGHT_PAREN,
                                curToken));
                        break;
                    case NUMERIC:
                        tokens.add(new Token(TokenTag.NUMBER, curToken));
                        break;
                    case ALPHABETIC:
                        boolean isFun = false;
                        for (Function fun : Function.values()) {
                            if (curToken.equals(fun.getStr())) {
                                tokens.add(new Token(TokenTag.FUN, curToken));
                                isFun = true;
                                break;
                            }
                        }
                        if (!isFun) {
                            tokens.add(new Token(TokenTag.VARIABLE, curToken));
                        }
                        break;
                    default:
                        throw new RuntimeException("Hm...");
                }
                curToken = "";
            }

            curToken += c;
        }
        return tokens.toArray(new Token[tokens.size()]);
    }

    private enum State {
        START,
        ALPHABETIC,
        NUMERIC,
        OPERATOR,
        PAREN,
        UNKNOWN,
        ERROR,
        END
    }

    private static State go(State curState, Character c) {
        if (c == null) {
            return State.END;
        }
        if (Character.isAlphabetic(c)) {
            return State.ALPHABETIC;
        }
        if (c == '(' || c == ')') {
            return State.PAREN;
        }
        if (c == '.' && curState.equals(State.NUMERIC)) {
            return State.NUMERIC;
        }
        if (Character.isDigit(c)) {
            if (curState.equals(State.ALPHABETIC)) {
                return State.ALPHABETIC;
            } else {
                return State.NUMERIC;
            }
        }
        // check if binary operator
        for (BinaryOperator bo : BinaryOperator.values()) {
            if (c == bo.getSymbol()) {
                if (curState.equals(State.OPERATOR)) {
                    return State.ERROR;
                }
                return State.OPERATOR;
            }
        }
        return State.UNKNOWN;
    }
}

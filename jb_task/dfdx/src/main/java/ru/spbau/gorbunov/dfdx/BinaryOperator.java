package ru.spbau.gorbunov.dfdx;

import java.security.InvalidParameterException;

/**
 * Created by Egor Gorbunov on 11.02.2016.
 * email: egor-mailbox@ya.ru
 */
public enum BinaryOperator {
    SUM('+', 2, false),
    SUB('-', 2, false),
    DIV('/', 3, false),
    MUL('*', 3, false);

    BinaryOperator(char symbol, int priority, boolean assoc) {
        this.symbol = symbol;
        this.priority = priority;
        this.assoc = assoc;
    }

    public char getSymbol() {
        return symbol;
    }

    public String getString() {
        return Character.toString(symbol);
    }

    public int getPriority() {
        return priority;
    }

    public boolean isRightAssoc() {
        return assoc;
    }

    public static BinaryOperator fromString(String str) {
        switch (str) {
            case "-":
                return SUB;
            case "+":
                return SUM;
            case "/":
                return DIV;
            case "*":
                return MUL;
            default:
                throw new InvalidParameterException("Bad input parameter; can't interpret as BinaryOperator.");

        }
    }

    private char symbol;
    private int priority;
    private boolean assoc; // false --> left, true --> right
}

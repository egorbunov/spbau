package ru.spbau.gorbunov.dfdx;

import java.security.InvalidParameterException;

/**
 * Created by Egor Gorbunov on 11.02.2016.
 * email: egor-mailbox@ya.ru
 */

/**
 * For now only one parameter function are used...
 */
public enum Function {
    LN("ln"),
    EXP("exp");

    Function(String str) {
        this.str = str;
    }

    public String getStr() {
        return str;
    }

    static Function fromString(String str) {
        switch (str) {
            case "ln":
                return LN;
            case "exp":
                return EXP;
            default:
                throw new InvalidParameterException("Bad input parameter; can't interpret as Function.");
        }
    }

    private final String str;
}

package ru.spbau.gorbunov.dfdx;

/**
 * Created by Egor Gorbunov on 11.02.2016.
 * email: egor-mailbox@ya.ru
 */
public class Token {
    public Token(TokenTag type, String token) {
        this.type = type;
        this.token = token;
    }

    public String getToken() {
        return token;
    }

    public TokenTag getType() {
        return type;
    }

    private TokenTag type;
    private String token;

    @Override
    public String toString() {
        return "[" + type.toString() + "] : " + token;
    }
}

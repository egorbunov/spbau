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

    public String getTokenStr() {
        return token;
    }

    public TokenTag getType() {
        return type;
    }

    private final TokenTag type;
    private final String token;

    @Override
    public boolean equals(Object obj) {
        if (! (obj instanceof Token)) {
            return false;
        }
        Token t = (Token) obj;
        return (t.getTokenStr().equals(token) && t.getType() == type);
    }

    @Override
    public String toString() {
        return "[" + type.toString() + "] : " + token;
    }
}

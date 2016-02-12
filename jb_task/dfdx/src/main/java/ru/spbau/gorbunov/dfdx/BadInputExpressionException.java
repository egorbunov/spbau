package ru.spbau.gorbunov.dfdx;

/**
 * Created by Egor Gorbunov on 12.02.16.
 * email: egor-mailbox@ya.ru
 */
public class BadInputExpressionException extends RuntimeException {
    public BadInputExpressionException() {
        super();
    }

    public BadInputExpressionException(String message) {
        super(message);
    }

    public BadInputExpressionException(String message, Throwable cause) {
        super(message, cause);
    }

    public BadInputExpressionException(Throwable cause) {
        super(cause);
    }

    protected BadInputExpressionException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}

package ru.spbau.gorbunov.dfdx;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import java.util.Arrays;
import java.util.Collection;

/**
 * Created by Egor Gorbunov on 12.02.16.
 * email: egor-mailbox@ya.ru
 */
@RunWith(Parameterized.class)
public class WrongInputTest {
    public WrongInputTest(String expr) {
        this.expr = expr;
    }

    @Parameterized.Parameters
    public static Collection testCases() {
        return Arrays.asList(
                "(1+2*3",
                ")(",
                "87634&^3"
        );
    }

    @Test(expected = BadInputExpressionException.class)
    public void wrongInputDetected() {
        Token[] tokens = Lexer.tokenize(expr);
        Parser.parse(tokens);
    }

    private final String expr;
}

package ru.spbau.gorbunov.dfdx;

/**
 * Created by Egor Gorbunov on 12.02.16.
 * email: egor-mailbox@ya.ru
 */

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import java.util.Arrays;
import java.util.Collection;

/**
 * Reversed polish notation test...
 */
@RunWith(Parameterized.class)
public class ParserTest {
    public ParserTest(String expression, String expectedRPN) {
        this.expression = expression;
        this.expectedRPN = expectedRPN;
    }

    @Parameterized.Parameters
    public static Collection testCases() {
        return Arrays.asList(new String[][] {
                {
                        "x+y",
                        "x y +"
                },
                {
                        "ln(x)",
                        "x ln"
                },
                {
                        "(var1+var2)*(var1-var2)",
                        "var1 var2 + var1 var2 - *"
                },
                {
                        "(var1+var2)*(var1-var2)-var1-var2/(var1-var2*var1)",
                        "var1 var2 + var1 var2 - * var1 - var2 var1 var2 var1 * - / -"
                },
                {
                        "ln((var1+var2)*(var1-var2)-var1-var2/(var1-var2*var1))",
                        "var1 var2 + var1 var2 - * var1 - var2 var1 var2 var1 * - / - ln"
                }
        });
    }

    @Test
    public void parsedCorrectly() {
        Assert.assertEquals(
                expectedRPN,
                Parser.parse(Lexer.tokenize(expression)).buildRPNStr()
        );
    }


    private final String expression;
    private final String expectedRPN;
}

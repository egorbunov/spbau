package ru.spbau.gorbunov.dfdx;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import java.util.Arrays;
import java.util.Collection;

/**
 * Created by Egor Gorbunov on 12.02.16.
 * email: egor-mailbox@ya.ru
 */

/*
    Simple testes for simplifier...
 */
@RunWith(Parameterized.class)
public class SimplifierTest {

    public SimplifierTest(String expr, String expectedRPM) {

        this.expr = expr;
        this.expectedRPM = expectedRPM;
    }


    @Parameterized.Parameters
    public static Collection testCases() {
        return Arrays.asList(new String[][]{
                {"5+10+10", "25"},
                {"0 * (x + exp(y) - 123123123)", "0"},
                {"1 * 1*1*1*1*1*1*1*x", "x"},
                {"1*0+exp(x+y)*0+0 / (ln(x))", "0"}
        });
    }

    @Test
    public void test() {
        MathExpressionTree tree = Parser.parse(Lexer.tokenize(expr));
        ExpressionSimplifier.simplify(tree);
        Assert.assertEquals(expectedRPM, tree.buildRPNStr());
    }

    private final String expr;
    private final String expectedRPM;
}

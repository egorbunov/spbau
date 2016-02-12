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

@RunWith(Parameterized.class)
public class LexerTest {
    public LexerTest(Token[] actualTokens, Token[] expectedTokens) {
        this.actualTokens = actualTokens;
        this.expectedTokens = expectedTokens;
    }

    @Parameterized.Parameters
    public static Collection testCases() {
        return Arrays.asList(new Token[][][]{
                {
                        Lexer.tokenize("1+2"),
                        {
                                new Token(TokenTag.NUMBER, "1"),
                                new Token(TokenTag.BINARY_OP, "+"),
                                new Token(TokenTag.NUMBER, "2")
                        }
                },
                {
                        Lexer.tokenize("( 1 + 2 ) * 3 - ( exp ( 5 ) + 7 )"),
                        {
                                new Token(TokenTag.LEFT_PAREN, "("),
                                new Token(TokenTag.NUMBER, "1"),
                                new Token(TokenTag.BINARY_OP, "+"),
                                new Token(TokenTag.NUMBER, "2"),
                                new Token(TokenTag.RIGHT_PAREN, ")"),
                                new Token(TokenTag.BINARY_OP, "*"),
                                new Token(TokenTag.NUMBER, "3"),
                                new Token(TokenTag.BINARY_OP, "-"),
                                new Token(TokenTag.LEFT_PAREN, "("),
                                new Token(TokenTag.FUN, "exp"),
                                new Token(TokenTag.LEFT_PAREN, "("),
                                new Token(TokenTag.NUMBER, "5"),
                                new Token(TokenTag.RIGHT_PAREN, ")"),
                                new Token(TokenTag.BINARY_OP, "+"),
                                new Token(TokenTag.NUMBER, "7"),
                                new Token(TokenTag.RIGHT_PAREN, ")")
                        }
                },
                {
                        Lexer.tokenize(" x1 * var + ( x2 - x1 ) * ( var + ln( var ) ) "),
                        {
                                new Token(TokenTag.VARIABLE, "x1"),
                                new Token(TokenTag.BINARY_OP, "*"),
                                new Token(TokenTag.VARIABLE, "var"),
                                new Token(TokenTag.BINARY_OP, "+"),
                                new Token(TokenTag.LEFT_PAREN, "("),
                                new Token(TokenTag.VARIABLE, "x2"),
                                new Token(TokenTag.BINARY_OP, "-"),
                                new Token(TokenTag.VARIABLE, "x1"),
                                new Token(TokenTag.RIGHT_PAREN, ")"),
                                new Token(TokenTag.BINARY_OP, "*"),
                                new Token(TokenTag.LEFT_PAREN, "("),
                                new Token(TokenTag.VARIABLE, "var"),
                                new Token(TokenTag.BINARY_OP, "+"),
                                new Token(TokenTag.FUN, "ln"),
                                new Token(TokenTag.LEFT_PAREN, "("),
                                new Token(TokenTag.VARIABLE, "var"),
                                new Token(TokenTag.RIGHT_PAREN, ")"),
                                new Token(TokenTag.RIGHT_PAREN, ")")

                        }
                },
                {
                        Lexer.tokenize("ln(2.73)"),
                        {
                                new Token(TokenTag.FUN, "ln"),
                                new Token(TokenTag.LEFT_PAREN, "("),
                                new Token(TokenTag.NUMBER, "2.73"),
                                new Token(TokenTag.RIGHT_PAREN, ")")
                        }
                }
        });
    }

    @Test
    public void test() {
        Assert.assertArrayEquals(expectedTokens, actualTokens);
    }


    private final Token[] actualTokens;
    private final Token[] expectedTokens;
}

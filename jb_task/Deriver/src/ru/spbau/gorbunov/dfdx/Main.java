package ru.spbau.gorbunov.dfdx;

import java.util.Arrays;

public class Main {

    public static void main(String[] args) {
        String expression = "x+y*10-(exp(x*y)+ln(x+y))";
        Token[] tokens = Lexer.tokenize(expression);
        System.out.println(Arrays.toString(tokens));
        ExpressionTree parse = Parser.parse(tokens);
        System.out.println(parse);
    }
}

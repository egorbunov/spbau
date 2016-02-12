package ru.spbau.gorbunov.dfdx;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class Main {

    public static void main(String[] args) {
        BufferedReader bufferRead = new BufferedReader(new InputStreamReader(System.in));
        while (true) {
            try {
                System.out.print("[expression] >> ");
                String expression = bufferRead.readLine();
                System.out.print("[variable]   >> ");
                String variable = bufferRead.readLine();
                try {
                    Token[] tokens = Lexer.tokenize(expression);
                    MathExpressionTree parse = Parser.parse(tokens);
                    ExpressionSimplifier.simplify(parse);
                    MathExpressionTree derivative = DerivativeCalculator.differentiate(parse, variable.trim());
                    ExpressionSimplifier.simplify(derivative);
                    System.out.println(derivative.buildExpressionStr());
                } catch (BadInputExpressionException e) {
                    System.out.println("Input error: " + e.getMessage());
                }
            } catch (IOException e) {
                break;
            }

        }
    }
}

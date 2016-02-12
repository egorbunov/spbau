package ru.spbau.gorbunov.dfdx;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.Formatter;
import java.util.Locale;

public class Main {

    public static void printGreeting() {
        System.out.println("***** Basic derivative calculator *****");
        System.out.println("Supported functions: "
                + Arrays.toString(
                Arrays.asList(MathFunction.values()).stream().map(MathFunction::getStr).toArray()));
        System.out.println("Supported binary operators: | symbol | priority | associativity |");
        System.out.println("                            .....................................");
        Arrays.asList(BinaryOperator.values()).stream().map(
                bo -> {
                    StringBuilder sb = new StringBuilder();
                    Formatter formatter = new Formatter(sb, Locale.US);
                    return "                             " +
                            formatter.format("%7s | %8s | %7s", bo.getString(),
                                    bo.getPriority(),
                                    bo.isRightAssoc() ? "right" : "left").toString();
                }
        ).forEach(System.out::println);
        System.out.println("Supported unary operators: no supported unary operators");
        System.out.println();
    }

    public static void main(String[] args) {
        printGreeting();
        BufferedReader bufferRead = new BufferedReader(new InputStreamReader(System.in));
        while (true) {
            try {
                System.out.print("[expression] >> ");
                String expression = bufferRead.readLine();
                if (expression == null) {
                    break;
                }
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
                System.out.println(e.getMessage());
                break;
            }
        }
        System.out.println("");
    }
}

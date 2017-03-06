package scala;

public final class InhSingleton implements Runnable {
    public static final InhSingleton object;

    static {
        object = new InhSingleton();
    }

    private InhSingleton() {
    }

    private int foo() {
        return 4;
    }

    @Override
    public void run() {
    }
}

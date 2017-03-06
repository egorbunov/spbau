package scala;

public abstract class BaseClass {
    public static void init(Base base) {
        base.setX(1);
    }

    public static int y(Base base) {
        return base.goo();
    }

    public static int goo(Base base) {
        return base.x() + 2;
    }
}

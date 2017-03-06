package scala;


public class Inh implements Base {
    private int x;
    private int y; // decompiler makes it final, but...
    private volatile boolean isYComputed;

    public Inh() {
        BaseClass.init(this);
    }

    @Override
    public int x() {
        return 0;
    }

    @Override
    public void setX(int val) {
        x = val;
    }

    private int computeLazyY() {
        synchronized (this) {
            if (!isYComputed) {
                y = BaseClass.y(this);
                isYComputed = true;
            }
            return y;
        }
    }

    @Override
    public int y() {
        return isYComputed ? this.y : this.computeLazyY();
    }

    @Override
    public int foo() {
        return 3;
    }

    @Override
    public int goo() {
        return BaseClass.goo(this);
    }
}

package queue;

public abstract class AbstractQueue implements Queue {
    protected int size;

    public void enqueue(Object obj) {
        assert obj != null;
        enqueueI(obj);
    }

    public Object dequeue() {
        assert !isEmpty();
        Object result = element();
        dequeueI();
        return result;
    }

    public Object element() {
        assert !isEmpty();
        return elementI();
    }

    public int size() {
        return sizeI();
    }

    public void clear() {
        clearI();
    }

    public boolean isEmpty() {
        return isEmptyI();
    }

    public void set(int index, Object obj) {
        assert obj != null;
        setI(index, obj);
    }

    public Object get(int index) {
        return getI(index);
    }

    public String toStr() {
        if (isEmpty()) {
            return "[]";
        }
        return toStrI();
    }

    protected abstract void enqueueI(Object obj);

    protected abstract void dequeueI();

    protected abstract Object elementI();

    protected abstract int sizeI();

    protected abstract void clearI();

    protected abstract boolean isEmptyI();

    protected abstract void setI(int index, Object obj);

    protected abstract Object getI(int index);

    protected abstract String toStrI();
}


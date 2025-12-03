package info.kgeorgiy.ja.mochekov.arrayset;

import java.util.*;

@SuppressWarnings("unused")
public class ArraySet<E> extends AbstractSet<E> implements NavigableSet<E> {

    private final List<E> array;
    private final Comparator<? super E> comparator;

    public ArraySet(Collection<? extends E> source, Comparator<? super E> comparator) {
        this.comparator = comparator;
        TreeSet<E> set = new TreeSet<>(comparator);
        set.addAll(source);
        this.array = new ArrayList<>(set);
    }

    public ArraySet(Collection<? extends E> source) {
        this(source, null);
    }

    public ArraySet(Comparator<? super E> comparator) {
        this(Collections.emptyList(), comparator);
    }

    public ArraySet() {
        this(Collections.emptyList(), null);
    }

    private ArraySet(List<E> array, Comparator<? super E> comparator) {
        this.array = array;
        this.comparator = comparator;
    }

    public ArraySet(SortedSet<E> set) {
        this.comparator = set.comparator();
        this.array = new ArrayList<>(set);
    }

    @Override
    public Iterator<E> iterator() {
        return Collections.unmodifiableCollection(array).iterator();
    }

    @Override
    public Iterator<E> descendingIterator() {
        return descendingSet().iterator();
    }

    @Override
    public Comparator<? super E> comparator() {
        return comparator;
    }

    @Override
    public NavigableSet<E> descendingSet() {
        return new ArraySet<>(array.reversed(), Collections.reverseOrder(comparator));
    }

    @Override
    public NavigableSet<E> subSet(E beginElement, boolean beginInclusive, E endElement, boolean endInclusive) {
        comparableExceptionCheck(beginElement, endElement);

        int beginIndex = findBorderIndex(beginElement, beginInclusive, true);
        int endIndex = findBorderIndex(endElement, endInclusive, false);

        return getterSubWaySet(beginIndex, endIndex);
    }

    @Override
    public NavigableSet<E> headSet(E endElement, boolean isInclusive) {
        return getterSubWaySet(0, findBorderIndex(endElement, isInclusive, false));
    }

    @Override
    public NavigableSet<E> tailSet(E beginElement, boolean inclusive) {
        return getterSubWaySet(findBorderIndex(beginElement, inclusive, true), array.size());
    }

    @Override
    public SortedSet<E> subSet(E beginElement, E endElement) {
        return subSet(beginElement, true, endElement, false);
    }

    @Override
    public SortedSet<E> headSet(E endElement) {
        return headSet(endElement, false);
    }

    @Override
    public SortedSet<E> tailSet(E beginElement) {
        return tailSet(beginElement, true);
    }

    @Override
    public E lower(E e) {
        return findElement(e, false, true);
    }

    @Override
    public E higher(E e) {
        return findElement(e, false, false);
    }

    @Override
    public E floor(E e) {
        return findElement(e, true, true);
    }

    @Override
    public E ceiling(E e) {
        return findElement(e, true, false);
    }

    @Override
    public E first() {
        return array.getFirst();
    }

    @Override
    public E last() {
        return array.getLast();
    }

    @Override
    @SuppressWarnings("unchecked")
    public boolean contains(Object obj) {
        return findRawIndex((E) obj) >= 0;
    }

    @Override
    public boolean retainAll(Collection<?> clt) {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean addAll(Collection<? extends E> clt) {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean removeAll(Collection<?> clt) {
        throw new UnsupportedOperationException();
    }

    @Override
    public E pollFirst() {
        throw new UnsupportedOperationException();
    }

    @Override
    public E pollLast() {
        throw new UnsupportedOperationException();
    }

    @Override
    public void clear() {
        throw new UnsupportedOperationException();
    }

    private NavigableSet<E> getterSubWaySet(int beginIndex, int endIndex) {
        if (beginIndex > endIndex) {
            return emptySet();
        } else {
            List<E> subList = array.subList(beginIndex, endIndex);
            return new ArraySet<>(subList, comparator);
        }
    }

    private E findElement(E e, boolean isInclude, boolean isLower) {
        int index = findRawIndex(e);

        if (index >= 0) {
            if (isInclude) {
                return getElementFromIndex(index);
            } else {
                return isLower ? (index > 0 ? getElementFromIndex(index - 1) : null)
                        : (index < array.size() - 1? getElementFromIndex(index + 1) : null);
            }
        } else {
            index = -index - 1;
            return isLower ? (index > 0 ? getElementFromIndex(index - 1) : null)
                    : (index <= array.size() - 1 ? getElementFromIndex(index) : null);
        }
    }

    private E getElementFromIndex(int index) {
        return array.get(index);
    }

    private int findBorderIndex(E e, boolean isInclusive, boolean isStart) {
        int ind = findRawIndex(e);
        if (ind < 0) {
            return -ind - 1;
        } else {
            return isStart ? (!isInclusive ? ind + 1 : ind) : (isInclusive ? ind + 1 : ind);
        }
    }

    private int findRawIndex(E e) {
        return Collections.binarySearch(array, e, comparator);
    }

    public int size() {
        return array.size();
    }

    public boolean isEmpty() {
        return size() == 0;
    }

    private NavigableSet<E> emptySet() {
        return new ArraySet<>(comparator);
    }

    @SuppressWarnings("unchecked")
    private void comparableExceptionCheck(E first, E second){
        if (comparator() != null && comparator().compare(first, second) > 0) {
            throw new IllegalArgumentException("key > toKey");
        } else if (comparator() == null) {
            if (((Comparable<? super E>) first).compareTo(second) > 0) {
                throw new IllegalArgumentException("key > toKey");
            }
        }
    }
}
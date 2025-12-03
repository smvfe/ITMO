package search;

public class BinarySearch3435 {
    // Hoare logic notation
    // P - precondition
    // Q - postcondition
    // I - invariant

    // a_1, ... , a_n --- sequence of integers
    // I: exist k in (1 <= k <= n):
    //    forall i in (1 <= i <= k): a_(i-1) < a_i &&
    //    forall i in (k < i < n - 1): a_i < a_(i+1)

    // func main
    // P: String[] args != null && args.length >= 1 && isInteger(args[i]) for each 0 <= i < args.length

    public static void main(String[] args) {
        // P: true
        int ArrayLength = args.length;
        int[] a = new int[ArrayLength];

        // I: for each j in 0 <= j < ArrayLength
        for (int i = 0; i < ArrayLength; i++) {
            // P: i' < ArrayLength
            a[i] = Integer.parseInt(args[i]);
            // Q: a[i'] = args[i']
        }

        int ans = binSearchIterativeFindMin(a);
        if (ans == binSearchRecursiveFindMin(a, 0, ArrayLength - 1)) {
            System.out.println(ans);
        }
    }

    // bs_iterative
    // P: a != null && a.length >= 1 && exists k:
    //    forall i in (1 <= i <= k): a[i-1] < a[i]  &&
    //    forall i in (k < i < a.length - 1): a[i] < a[i+1]
    // Q: pivot = ans in (0 <= ans < a.length) : a[pivot] - min
    public static int binSearchIterativeFindMin(int[] a) {
        // P: true
        int left = 0;
        int right = a.length - 1;

        // P: a[i] < a[a.length - 1] for i in  (left <= i < right)
        if (a[left] < a[right]) {
            // Q: pivot = left
            return a[left];
        }

        // I: a[left'] >= ans <= a[right']
        //    && left' < right'
        //    && 0 <= left' < right' < a.length
        while (left < right) {
            // P: left' < right' && a[left'] >= ans <= a[right']
            int middle = left + (right - left) / 2;

            if (a[right] < a[middle]) {
                // P: a[right] < a[middle] && a[middle'] >= ans <= a[right']
                left = middle + 1;
                // Q: left' = middle'
            } else {
                // P: a[middle] < a[right] && a[left'] >= ans <= a[right']
                right = middle;
                // Q: right' = middle'
            }
            // Q: a[left'] >= ans <= a[right']
        }
        // Q: pivot == left && left == right && a[left] - min
        return a[left];
    }

    // bs_recursive
    // P: isInteger(x) && isInteger(left) && isInteger(right)
    //    && a != null && a.length >= 1
    //    && exists k:
    //       forall i in (1 <= i <= k): a[i-1] < a[i]  &&
    //       forall i in (k < i < a.length - 1): a[i] < a[i+1]
    //    && 0 <= left <= k <= right < a.length
    // Q: pivot = k in (0 <= k < a.length)
    //    && a[pivot] - min
    public static int binSearchRecursiveFindMin(int[] a, int left, int right) {
        // P: left == right
        if (left == right) {
            // Q: a[left] - min
            return a[left];
        }

        // P: a_i <= a_(i+1) for (left <= i < right)
        if (a[left] < a[right]) {
            // Q: a[left] - min
            return a[left];
        }

        // P: true
        int middle = left + (right - left) / 2;

        if (a[middle] > a[right]) {
            // P: left' = middle' && a[right] < a[middle]
            return binSearchRecursiveFindMin(a, middle + 1, right);
            // Q: return ... - min
        }

        // P: right' = middle' && a[middle] < a[right]
        return binSearchRecursiveFindMin(a, left, middle);
        // Q: return ... - min
    }
}

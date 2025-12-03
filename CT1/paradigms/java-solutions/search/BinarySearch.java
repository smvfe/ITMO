package search;

public class BinarySearch {
    // Hoare logic notation
    // P - precondition
    // Q - postcondition
    // I - invariant

    // a_1, ... , a_n --- sequence of integers
    // I: a_(i+1) <= a_i for (1 <= i <= n)

    // func main
    // P: String[] args != null && args.length >= 2 && isInteger(args[i]) for each 1 <= i < args.length

    public static void main(String[] args) {
        // P: true
        int ArrayLength = args.length - 1;
        int x = Integer.parseInt(args[0]);
        int[] a = new int[ArrayLength];

        // I: for each j in 0 < j < args.length : a[ArrayLength + 1] <= args[j] <= a[0]
        for (int i = 1; i < args.length; i++) {
            // P: i' < ArrayLength && a[i'] == null
            a[i-1] = Integer.parseInt(args[i]);
            // Q: a[i'] <= a[i]
        }

        int ans = binSearchIterative(a, x);
        if (ans == binSearchRecursive(a, x, 0, a.length)) {
            System.out.println(ans);
        }
    }

    // bs_iterative
    // P: isInteger(x) && a != null
    // Q: pivot = ind in (0 <= ind < a.length) 
    //    && a[ind] <= x 
    //    && No ind_t in (0 <= ind_t < a.length): ind_t <= ind && a[ind_t] <= x
    public static int binSearchIterative(int[] a, int x) {
        // P: true
        int left = -1;
        int right = a.length;

        // I: a[right'] <= x < a[left']
        while (left + 1 < right) {
            // P: left' + 1 < right' && a[right'] <= x < a[left']
            int middle = left + (right - left) / 2;

            if (x < a[middle]) {
                // P: left <= middle' <= right && a[right'] <= x < a[middle'] 
                left = middle;
                // Q: left' = middle' && right in (left'< right <= right')
            } else {
                // P: left <= middle' < right &&  a[middle'] <= x < a[left']
                right = middle;
                // Q: right' = middle' && left in (left' < left <= right')
            }
            // Q: pivot in (left' < pivot <= right') && a[right'] <= x < a[left'] 
        }
        // P: a[right'] <= x < a[left'] && left' + 1 >= right'
        return right;
        // Q: pivot = right' && a[right'] <= x < a[left']
    }

    // bs_recursive
    // P: isInteger(x) && isInteger(left) && isInteger(right)
    //    && a != null 
    //    && 0 <= left <= right < a.length
    //    && a[right]<= x < a[left]
    // Q: pivot = ind in (0 <= ind < a.length)
    //    && a[ind] <= x 
    //    && No ind_t in (0 <= ind_t < a.length) : ind_t <= ind && a[id1] <= x
    public static int binSearchRecursive(int[] a, int x, int left, int right) {
        if (left >= right) {
            // P: left > right && a[right'] <= x < a[left']
            return left;
            // Q: pivot = left' && a[left'] <= x < a[right']
        } else {
            // P: right > left && a[right'] <= x < a[left']
            int middle = left + (right - left) / 2;

            if (x < a[middle]) {
                // P: left <= middle <= right && a[right] <= x < a[middle]
                return binSearchRecursive(a, x, middle + 1, right);
                // Q: pivot = return ...
            } else {
                // P: left' <= middle' <= right' && a[middle'] <= x < a[left']
                return binSearchRecursive(a, x, left, middle);
                // Q: pivot = return ...
            }
        }
    }
}
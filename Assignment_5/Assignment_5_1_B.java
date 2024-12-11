package Assignment_5;

import java.util.Arrays;

public class Assignment_5_1_B {
    public static int[] merge(int[] xs, int[] ys) {
        int[] result = new int[xs.length + ys.length];
        int i = 0, j = 0, k = 0;

        // Merge the two arrays while both have elements.
        while (i < xs.length && j < ys.length) {
            if (xs[i] < ys[j]) {
                result[k++] = xs[i++];
            } else {
                result[k++] = ys[j++];
            }
        }

        // Add remaining elements of xs, if any.
        while (i < xs.length) {
            result[k++] = xs[i++];
        }

        // Add remaining elements of ys, if any.
        while (j < ys.length) {
            result[k++] = ys[j++];
        }

        return result;
    }

    public static void main(String[] args) {
        int[] xs = { 3, 5, 12 };
        int[] ys = { 2, 3, 4, 7 };
        int[] result = merge(xs, ys);
        System.out.println(Arrays.toString(result)); // Output: [2, 3, 3, 4, 5, 7, 12]
    }
}
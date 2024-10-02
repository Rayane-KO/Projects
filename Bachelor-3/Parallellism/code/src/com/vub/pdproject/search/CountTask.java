//net-id: rkouidan
package com.vub.pdproject.search;
import com.vub.pdproject.Util;

import java.util.Arrays;
import java.util.List;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.RecursiveTask;

/**
 * Count occurences in parallel
 * @Author Rayane Kouidane 0587073
 */

public class CountTask extends RecursiveTask<Integer> {
    String keyword;
    String[] text;
    int start;
    int end;
    int T;

    /**
     * Recursive task for counting occurrences in parallel
     * @param keyword A single keyword (without spaces)
     * @param text The text to count the occurrences
     * @param start Where to start counting
     * @param end Where to end counting
     * @param T Sequential threshold for counting occurrences
     */
    public CountTask(String keyword, String[] text, int start, int end, int T) {
        this.keyword = keyword;
        this.text = text;
        this.start = start;
        this.end = end;
        this.T = T;
    }

    @Override
    protected Integer compute() {
        if (end - start <= T) {
            return countOccurrencesSeq(keyword, text, start, end);
        } else {
            int mid = (start + end) / 2;
            CountTask leftTask = new CountTask(keyword, text, start, mid, T);
            CountTask rightTask = new CountTask(keyword, text, mid, end, T);
            leftTask.fork();
            int rightResult = rightTask.compute();
            int leftResult = leftTask.join();
            return leftResult + rightResult;
        }
    }


    public static int countOccurrencesSeq(String keyword, String[] text, int start, int end){
        int count = 0;
        for (int i = start; i < end; i++){
            count += countOccurrences(keyword, text[i]); // Counts occurrences from start to end
        }
        return count;
    }

    public static int countOccurrences(String keyword, String text) {
        int count = 0;
        int k = 0;
        for (int i = 0; i < text.length(); i++) {
            if (Util.isWhitespaceOrPunctuationMark(text.charAt(i))) {
                if (k == keyword.length()) {
                    count++;
                }
                k = 0;
            } else if (k >= 0) {
                if (k < keyword.length() && text.charAt(i) == keyword.charAt(k)) {
                    k++;
                } else {
                    k = -1;
                }
            }
        }

        if (k == keyword.length()) {
            count++;
        }
        return count;
    }
}




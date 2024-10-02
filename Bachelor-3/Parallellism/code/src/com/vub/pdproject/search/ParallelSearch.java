//net-id: rkouidan
package com.vub.pdproject.search;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.RecursiveTask;
import com.vub.pdproject.Util;

import com.vub.pdproject.data.WikipediaData;
import com.vub.pdproject.data.models.Article;

import static java.util.concurrent.ForkJoinTask.getPool;

/**
 * A parallel implementation of QueryEngine using Java Fork-join
 * @author Rayane Kouidane 0587073
 */

public class ParallelSearch implements QueryEngine {
    final int p; // Parallelism level (i.e. max. # cores that can be used by Java Fork/Join)
    final int T; // Sequential threshold for search
    final int T2; // Sequential threshold for counting occurrences
    final ForkJoinPool pool; // Pool for the parallel tasks

    /**
     * Creates a parallel search engine with p worker threads.
     * Counting occurrences is to be done sequentially (T ~ +inf)
     *
     * @param p parallelism level
     */
    public ParallelSearch(int p) {
        this(p, Integer.MAX_VALUE, Integer.MAX_VALUE);
    }

    /**
     * Creates a parallel search engine with p worker threads and sequential cut-off threshold T.
     *
     * @param p parallelism level
     * @param T sequential threshold
     */
    public ParallelSearch(int p, int T, int T2) {
        this.p = p;
        this.T = T;
        this.T2 = T2;
        this.pool = new ForkJoinPool(p);
    }

    @Override
    public List<RRecord> search(String keyword, WikipediaData data) {
        SearchTask task = new SearchTask(keyword, data, 0, data.getArticleIDs().size());
        return pool.invoke(task);
    }

    private class SearchTask extends RecursiveTask<List<RRecord>> {
        String keyword;
        WikipediaData data;
        int start;
        int end;

        /**
         * Recursive task for parallel search
         *
         * @param keyword A single keyword (without spaces)
         * @param data The data to be searched
         * @param start Where to start the search
         * @param end Where to end the search
         */
        public SearchTask(String keyword, WikipediaData data, int start, int end) {
            this.keyword = keyword;
            this.data = data;
            this.start = start;
            this.end = end;
        }

        @Override
        protected List<RRecord> compute() {
            if (end - start <= T) {
                List<RRecord> res = evaluateRange(keyword, data, start, end);
                Collections.sort(res);
                return res;
            } else {
                int mid = (start + end) / 2;
                SearchTask leftTask = new SearchTask(keyword, data, start, mid);
                SearchTask rightTask = new SearchTask(keyword, data, mid, end);
                leftTask.fork();
                List<RRecord> rightResult = rightTask.compute();
                List<RRecord> leftResult = leftTask.join();
                return sortedMerge(leftResult, rightResult);
            }
        }

        public int countOccurrences(String keyword, String text) {
            String[] chunks = text.split(" ");
            CountTask task = new CountTask(keyword, chunks, 0, chunks.length, T2);
            return pool.invoke(task);
        }

        public double evaluate_relevance(String keyword, String articleID, WikipediaData data) {
            // fetch data for article
            Article article = data.getArticle(articleID);

            // check how many times query string appears in text
            int occurrences = 0;
            occurrences += countOccurrences(keyword, article.text);

            // calculate relevance score
            double relevance_score = 0;
            if (countOccurrences(keyword, article.title) > 0)
                relevance_score = 0.5;

            relevance_score += 1.5 * occurrences / (occurrences + 20);
            return relevance_score;
        }

        private List<RRecord> evaluateRange(String keyword, WikipediaData data, int start, int end) {
            List<RRecord> res = new ArrayList<RRecord>();
            for (int i = start; i < end; i++) {
                String articleId = data.getArticleIDs().get(i);
                double relevance = evaluate_relevance(keyword, articleId, data);
                if (relevance > 0) {
                    res.add(new RRecord(articleId, relevance));
                }
            }
            return res;
        }

        /**
         * Merges and sorts list1 and list2
         *
         * @param list1 first list to merge
         * @param list2 second list to merge
         */
        private List<RRecord> sortedMerge(List<RRecord> list1, List<RRecord> list2) {
            List<RRecord> res = new ArrayList<RRecord>(list1.size() + list2.size());
            int idx1 = 0;
            int idx2 = 0;
            while (idx1 < list1.size() && idx2 < list2.size()) {
                RRecord rec1 = list1.get(idx1);
                RRecord rec2 = list2.get(idx2);
                if (rec1.compareTo(rec2) < 0) {
                    res.add(rec1);
                    idx1++;
                } else {
                    res.add(rec2);
                    idx2++;
                }
            }
            while (idx1 < list1.size()){
                res.add(list1.get(idx1));
                idx1++;
            }
            while (idx2 < list2.size()){
                res.add(list2.get(idx2));
                idx2++;
            }
            return res;
        }
    }
}


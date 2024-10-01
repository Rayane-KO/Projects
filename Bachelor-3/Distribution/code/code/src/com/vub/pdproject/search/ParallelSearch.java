package com.vub.pdproject.search;

import java.util.List;

import com.vub.pdproject.data.WikipediaData;

/**
 * TODO: A parallel implementation of QueryEngine using Java Fork-join
 * (see assignment for detailed requirements of this implementation)
 * <p>
 * This is normally the only file you should change (except for Main.java for testing/evaluation).
 * If you for some reason feel the need to change another existing file, contact Aäron Munsters first,
 * and mention this modification explicitly in the report.
 * <p>
 * Note that adding new files for modularity purposes is always ok.
 *
 * @author You
 */

@SuppressWarnings("unused") // TODO: remove this line once you put this class to use
public class ParallelSearch implements QueryEngine {
    final int p; // Parallelism level (i.e. max. # cores that can be used by Java Fork/Join)
    final int T; // Sequential threshold (semantics depend on your cut-off implementation)

    /**
     * Creates a parallel search engine with p worker threads.
     * Counting occurrences is to be done sequentially (T ~ +inf)
     *
     * @param p parallelism level
     */
    ParallelSearch(int p) {
        this(p, Integer.MAX_VALUE);
    }

    /**
     * Creates a parallel search engine with p worker threads and sequential cut-off threshold T.
     *
     * @param p parallelism level
     * @param T sequential threshold
     */
    public ParallelSearch(int p, int T) {
        this.p = p;
        this.T = T;
        // Hint: Initialise the Java Fork/Join framework here as well.
    }

    @Override
    public List<RRecord> search(String keyword, WikipediaData data) {
        // TODO: implement this method using Java Fork-Join
        return null;
    }
}


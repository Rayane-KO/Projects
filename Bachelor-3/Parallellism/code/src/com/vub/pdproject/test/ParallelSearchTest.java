//net-id: rkouidan
package com.vub.pdproject.test;

import com.vub.pdproject.data.WikipediaData;
import com.vub.pdproject.search.ParallelSearch;
import com.vub.pdproject.search.QueryEngine;
import com.vub.pdproject.search.SequentialSearch;
import com.vub.pdproject.search.WikipediaQuery;
import org.junit.Assert;
import org.junit.Test;

import java.io.IOException;
import java.util.List;

/**
 * Unit tests for the parallel search
 * @Author Rayane Kouidane 0587073
 */

public class ParallelSearchTest {
    private Boolean sameRRecords(List<QueryEngine.RRecord> lr1, List<QueryEngine.RRecord> lr2){
        if (lr1.size() != lr2.size()) {
            return false;
        }
        int size = lr1.size();
        for (int i=0; i < size; i++){
            QueryEngine.RRecord r1 = lr1.get(i);
            QueryEngine.RRecord r2 = lr2.get(i);
            if (r1.compareTo(r2) != 0){
                return false;
            }
        }
        return true;
    }

    private Boolean checkSameResult(WikipediaData.WikipediaDataSet benchmark) throws IOException{
        WikipediaQuery query = WikipediaQuery.forBenchmark(benchmark);
        QueryEngine qes = new SequentialSearch();
        QueryEngine qep = new ParallelSearch(20, 1, Integer.MAX_VALUE);
        assert query != null;
        List<QueryEngine.RRecord> resultSequential = query.execute(qes);
        List<QueryEngine.RRecord> resultParallel = query.execute(qep);
        return sameRRecords(resultSequential, resultParallel);
    }

    @Test
    public void givenSmallQuery_whenParallelSearch_thenReturnSameAsSequential() throws IOException {
        WikipediaData.WikipediaDataSet benchmark = WikipediaData.WikipediaDataSet.Small;
        Assert.assertTrue(checkSameResult(benchmark));
    }

    @Test
    public void givenMediumQuery_whenParallelSearch_thenReturnSameAsSequential() throws IOException {
        WikipediaData.WikipediaDataSet benchmark = WikipediaData.WikipediaDataSet.Medium;
        Assert.assertTrue(checkSameResult(benchmark));
    }

    @Test
    public void givenLargeQuery_whenParallelSearch_thenReturnSameAsSequential() throws IOException {
        WikipediaData.WikipediaDataSet benchmark = WikipediaData.WikipediaDataSet.Large;
        Assert.assertTrue(checkSameResult(benchmark));
    }
}

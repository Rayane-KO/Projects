//net-id: rkouidan
package com.vub.pdproject;

import com.vub.pdproject.data.WikipediaData.WikipediaDataSet;
import com.vub.pdproject.search.ParallelSearch;
import com.vub.pdproject.search.QueryEngine;
import com.vub.pdproject.search.QueryEngine.RRecord;

import com.vub.pdproject.search.SequentialSearch;
import com.vub.pdproject.search.WikipediaQuery;
import org.openjdk.jmh.annotations.*;
import org.openjdk.jmh.infra.Blackhole;

import java.io.FileWriter;
import java.io.IOException;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.openjdk.jmh.results.RunResult;
import org.openjdk.jmh.results.format.ResultFormatType;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;
import org.openjdk.jmh.util.Statistics;

/**
 * Benchmarks for the parallel search
 * @Author Rayane Kouidane 0587073
 */

public class ParallelBenchmark {

    public static void main(String[] args) throws Exception {
        Options options = new OptionsBuilder()
                .include(ParallelBenchmark.class.getSimpleName())
                .resultFormat(ResultFormatType.CSV)
                .result("benchmark_best_large.csv")
                .shouldDoGC(true)
                .build();
        Collection<RunResult> results = new Runner(options).run();
        addOtherStats(results);
    }

    /**
     * @param results Results from the benchmarking
     * Used sources:
     * https://stackoverflow.com/questions/2885173/how-do-i-create-a-file-and-write-to-it
     * https://help.sap.com/doc/60a4f4fc10a14e20b47d25ec00841891/4.2.5/en-US/com/businessobjects/sdk/lcm/RunResult.html
     */
    private static void addOtherStats(Collection<RunResult> results) throws IOException {
        FileWriter writer = new FileWriter( "benchmark_best_large_extra.csv");
        writer.append("stdev,variance\n");
        for (RunResult result : results) {
            Statistics stats = result.getPrimaryResult().getStatistics();
            double stdev = stats.getStandardDeviation();
            double var = stats.getVariance();
            writer.write(String.valueOf(stdev));
            writer.write(",");
            writer.write(String.valueOf(var));
            writer.write("\n");
        }
        writer.close();
    }

    @State(Scope.Benchmark)
    public static class FirstSearchState {
        private WikipediaQuery wikipediaQuery;
        private QueryEngine queryEngine;
        @Setup(Level.Trial)
        public void doSetup() throws IOException {
            wikipediaQuery = WikipediaQuery.forBenchmark(WikipediaDataSet.Large);
            queryEngine = new ParallelSearch(p, searchCutoff, countCutoff);
        }

        @Param({"1", "4", "8", "16", "20"})
        int p;

        @Param({"1"})
        int searchCutoff;

        @Param({"1", "10", "100", "1000", "10000", "100000", "150000", "194969"})
        int countCutoff;
    }

    @Benchmark
    @BenchmarkMode({Mode.AverageTime})
    @OutputTimeUnit(TimeUnit.MILLISECONDS)
    @Fork(1)
    @Threads(1)
    @Warmup(iterations = 5, time = 500, timeUnit = TimeUnit.MILLISECONDS)
    @Measurement(iterations = 10, time = 500, timeUnit = TimeUnit.MILLISECONDS)
    public void testSearchParallelSearch(Blackhole bh, FirstSearchState state) throws IOException {
        List<RRecord> result_article_ids = state.wikipediaQuery.execute(state.queryEngine);
        bh.consume(result_article_ids);
    }
}


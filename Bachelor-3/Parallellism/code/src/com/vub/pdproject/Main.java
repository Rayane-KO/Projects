//net-id: rkouidan
package com.vub.pdproject;

import com.vub.pdproject.data.WikipediaData;
import com.vub.pdproject.data.WikipediaData.WikipediaDataSet;
import com.vub.pdproject.search.ParallelSearch;
import com.vub.pdproject.search.QueryEngine;
import com.vub.pdproject.search.QueryEngine.RRecord;
import com.vub.pdproject.search.SequentialSearch;
import com.vub.pdproject.search.WikipediaQuery;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;

/**
 * Main
 * @Author Rayane Kouidane 0587073
 */

public class Main {
    public static void main(String[] args) throws IOException {
        // Benchmark to be used (`Small`, `Medium`, `Large` on your machine or `Firefly` on Firefly)
        WikipediaDataSet benchmark =;


        // Prints out some information about this benchmark
        System.out.println("*** QUERY ***");
        System.out.println(query);

        System.out.println();
        System.out.println("*** PARALLEL SEARCH ***");

        QueryEngine qe = new ParallelSearch(20, 1, 500);
        assert query != null;
        executeQuery(qe, query);

        System.out.println("*** SEQUENTIAL SEARCH ***");
        QueryEngine qs = new SequentialSearch();
        executeQuery(qs, query);
        System.out.println(query.getData().getArticleIDs().size());
        System.out.println(getLongestArticleLength(query));
    }

    public static int getLongestArticleLength(WikipediaQuery query) {
        int maxLength = 0;  // Variable to store the maximum length found
        for (String articleId : query.getData().getArticleIDs()) {
            String articleText = query.getData().getArticle(articleId).text;  // Assuming getFullText() returns the full text of the article
            if (articleText.length() > maxLength) {
                maxLength = articleText.length();
            }
        }
        return maxLength;
    }

    public static void executeQuery(QueryEngine qe, WikipediaQuery query){
        // Execute query using query engine
        long start = System.currentTimeMillis();
        List<RRecord> result_article_ids = query.execute(qe);
        long end = System.currentTimeMillis();
        long duration = end - start;

        // Output the result of the query (names of articles and their relevance, ordered by decreasing relevance)


        System.out.println("*** RESULT ***");
        int i = 1;
        for (RRecord result_article_id : result_article_ids) {
            System.out.println(i + " ) " +
                    query.getData().getArticle(result_article_id.articleID).title +
                    " (" + result_article_id.relevance_score + ")");
            i++;
        }
        System.out.println("Execution time: " + duration + " ms");
        System.out.println();
    }
}

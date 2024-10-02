package com.vub.pdproject.data;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.vub.pdproject.data.models.Article;
import com.vub.pdproject.data.readers.ArticleReader;

/**
 * Class representing a subset of Yelp data.
 *
 * @author Aäron Munsters
 */
public class WikipediaData {
    private static final String preset_loc = "data/presets"; // root folder containing data presets
    private final Map<String, Article> articles; // Articles map: Mapping from article ID to its data
    private final List<String> articleIDs; // a list of all article IDs

    public enum WikipediaDataSet {
        Small,
        Medium,
        Large,
        Firefly,
    }


    protected WikipediaData(Map<String, Article> articles) {
        this.articles = articles;
        this.articleIDs = new ArrayList<>(articles.keySet());
    }

    /**
     * @return a list of all article IDs (in O(1))
     */
    public List<String> getArticleIDs() {
        return articleIDs;
    }

    /**
     * @param aid article ID
     * @return data for article with ID
     */
    public Article getArticle(String aid) {
        return articles.get(aid);
    }

    public String toString() {
        long article_shortest = Integer.MAX_VALUE;
        long article_longest = -1;
        long articles_length_total = 0;
        long total_articles = 0;
        for (Article article : articles.values()) {
            total_articles += 1;
            article_shortest = Math.min(article_shortest, article.text.length());
            article_longest = Math.max(article_longest, article.text.length());
            articles_length_total += article.text.length();
        }

        return "# articles: " + total_articles + System.lineSeparator()
                + "# characters: " + articles_length_total + System.lineSeparator()
                + "avg. # characters per article: " + (double) articles_length_total / total_articles + " (shortest: " + article_shortest + ", longest: " + article_longest + ")";
    }

    /**
     * @param wikipediaDataSet The target data preset to load.
     * @return the data preset with given index.
     */
    static public WikipediaData forPreset(WikipediaDataSet wikipediaDataSet) throws IOException {
        String target = null;
        switch (wikipediaDataSet) {
            case Small:
                target = "dataset1";
                break;
            case Medium:
                target = "dataset2";
                break;
            case Large:
                target = "dataset3";
                break;
            case Firefly:
                throw new IOException("Full data set not available as local preset");
        }
        assert target != null;
        Map<String, Article> articles = ArticleReader.readData(preset_loc + "/" + target);
        return new WikipediaData(articles);
    }

    /**
     * @return The data from the dataset on Serenity.
     */
    static public WikipediaData forFirefly() throws IOException {
        Map<String, Article> articles = ArticleReader.readData("/data/PD/wikipedia2024");
        return new WikipediaData(articles);
    }
}

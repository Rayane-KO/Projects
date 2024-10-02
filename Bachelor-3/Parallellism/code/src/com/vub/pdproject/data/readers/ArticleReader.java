package com.vub.pdproject.data.readers;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonElement;
import com.google.gson.JsonStreamParser;
import com.vub.pdproject.data.models.Article;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;

public class ArticleReader {
    static public Map<String, Article> readData(String dataPath) throws IOException {
        // Read directory for Wikipedia files
        File directory = new File(dataPath);
        File[] articlesJsons = directory.listFiles();
        if (articlesJsons == null) {
            System.out.println("Could not locate following as directory on the system: " + directory.getPath());
            System.out.println("Exiting now.");
            System.exit(0);
        }

        Map<String, Article> articles = new HashMap<>();

        int readSoFar = 0;
        int readTotal = articlesJsons.length;
        for (File articlesJson : articlesJsons) {
            System.out.println("Reading dataset " + articlesJson.getPath() + " (" + readSoFar++ + "/" + readTotal + ")");
            // Read all articles
            InputStream dataStream = getInputStream(articlesJson.getPath());
            articles.putAll(readJsonStream(dataStream));
        }

        return articles;
    }

    /**
     * @param inputStream A stream of Wikipedia data
     * @return A mapping from id to articles, converted from entries on the stream of Wikipedia data
     */
    static private Map<String, Article> readJsonStream(InputStream inputStream) throws IOException {
        return readJsonStream(inputStream, article -> true);
    }


    /**
     * @param inputStream   A stream of Wikipedia data
     * @param shouldInclude A lambda that determines whether a particular article should be included in the result set
     * @return A mapping from id to articles, converted from entries on the stream of Wikipedia data
     */
    static private Map<String, Article> readJsonStream(InputStream inputStream, Function<Article, Boolean> shouldInclude) throws IOException {
        Gson gson = createBuilder();
        Reader reader = new InputStreamReader(inputStream, StandardCharsets.UTF_8);
        JsonStreamParser parser = new JsonStreamParser(reader);

        HashMap<String, Article> articles = new HashMap<>();
        while (parser.hasNext()) {
            JsonElement e = parser.next();
            assert e.isJsonArray();
            for (JsonElement element : e.getAsJsonArray()) {
                Article article = gson.fromJson(element, Article.class);
                if (shouldInclude.apply(article))
                    articles.put(article.id, article);
            }
        }

        reader.close();
        return articles;
    }


    /**
     * Create a builder for JSON Wikipedia data
     *
     * @return a GSON builder for JSON Wikipedia data
     */
    static private Gson createBuilder() {
        GsonBuilder builder = new GsonBuilder();
        return builder.create();
    }

    /**
     * Given a path to a file, turn it into a stream of Wikipedia data
     *
     * @param dataPath Path to a file
     * @return A stream of Wikipedia data
     */
    static private InputStream getInputStream(String dataPath) throws IOException {
        return Files.newInputStream(Paths.get(dataPath));
    }

}

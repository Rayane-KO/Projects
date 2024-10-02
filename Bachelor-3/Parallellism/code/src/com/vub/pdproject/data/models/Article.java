package com.vub.pdproject.data.models;

import com.google.gson.annotations.SerializedName;

public class Article {

    // A unique identifier for this article
    @SerializedName("id")
    public String id;

    // The article title
    @SerializedName("title")
    public String title;

    // The article content
    @SerializedName("text")
    public String text;
}

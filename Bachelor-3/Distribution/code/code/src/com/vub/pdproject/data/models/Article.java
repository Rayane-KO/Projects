package com.vub.pdproject.data.models;

import com.google.gson.annotations.SerializedName;

import java.io.Serializable;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Article implements Serializable {

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

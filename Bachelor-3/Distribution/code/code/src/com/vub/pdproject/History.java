// rkouidan
package com.vub.pdproject;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Stores the changes made to an article
 */
public class History {
    private int currentVersion = 0;
    private final ConcurrentHashMap<Integer, String> history;
    public History(){
        this.history = new ConcurrentHashMap<>();
    }

    /**
     * Adds an entry to the history of changes
     * @param content The new content that was added
     */
    public synchronized void addEntry(String content){
        history.put(currentVersion, content);
        currentVersion++;
    }

    /**
     * Returns the history
     */
    public ConcurrentHashMap<Integer, String> getHistory(){
        return history;
    }
}

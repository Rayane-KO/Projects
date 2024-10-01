// rkouidan
package com.vub.pdproject.tests;

import com.vub.pdproject.History;
import org.junit.Test;
import org.junit.jupiter.api.AfterEach;
import static org.junit.Assert.assertEquals;

public class HistoryTest {
    private History history = new History();

    @AfterEach
    public void setUp() {
        history = new History();
    }

    @Test
    public void testHistoryEmptyAtStart() {
        assertEquals(history.getHistory().size(), 0);
    }

    @Test
    public void testAddEntry() {
        history.addEntry("content");
        assertEquals(history.getHistory().size(), 1);
        assertEquals(history.getHistory().get(0), "content");
    }
}

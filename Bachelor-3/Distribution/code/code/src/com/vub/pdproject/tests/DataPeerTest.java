// rkouidan
package com.vub.pdproject.tests;

import com.google.gson.Gson;
import com.vub.pdproject.DataPeer;
import com.vub.pdproject.History;
import com.vub.pdproject.data.models.Article;
import org.junit.BeforeClass;
import org.junit.Test;

import javax.crypto.SecretKey;
import java.rmi.RemoteException;
import java.security.GeneralSecurityException;

import static com.vub.pdproject.utils.SecurityUtil.*;
import static org.junit.Assert.*;

public class DataPeerTest {
    private static DataPeer dataPeer;
    private static DataPeer smallDataPeer;
    private static SecretKey sessionKey;
    private static Gson gson;

    @BeforeClass
    public static void setUp() throws RemoteException, GeneralSecurityException {
        dataPeer = new DataPeer(20);
        smallDataPeer = new DataPeer(1);
        sessionKey = generateSessionKey();
        String encodedKey = byteToString(encryptKey(sessionKey.getEncoded(), dataPeer.getKey()));
        String encodedKey2 = byteToString(encryptKey(sessionKey.getEncoded(), smallDataPeer.getKey()));
        dataPeer.setSessionKey(encodedKey);
        smallDataPeer.setSessionKey(encodedKey2);
        gson = new Gson();
    }

    @Test
    public void testAddArticle() throws GeneralSecurityException, RemoteException {
        String title = "title";
        String content = "content";
        String encryptedTitle = encryptData(title, sessionKey);
        String encryptedContent = encryptData(content, sessionKey);
        dataPeer.add(encryptedTitle, encryptedContent);

        Article article = gson.fromJson(decryptData(dataPeer.get(encryptedTitle), sessionKey), Article.class);
        assertNotNull(article);
        assertEquals(article.title, title);
        assertEquals(article.text, content);
    }

    @Test
    public void testAddArticleFullDataPeer() throws GeneralSecurityException, RemoteException {
        String title = "title2";
        String content = "content";
        String encryptedTitle = encryptData(title, sessionKey);
        String encryptedContent = encryptData(content, sessionKey);
        smallDataPeer.add(encryptedTitle, encryptedContent);

        String title2 = encryptData("title2", sessionKey);
        String content2 = encryptData("content2", sessionKey);
        try {
            smallDataPeer.add(title2, content2);
        } catch (RemoteException e) {
            assertEquals(e.getMessage(), "Capacity exceeded");
        }
    }

    @Test
    public void testEditArticle() throws GeneralSecurityException, RemoteException {
        String title = "title3";
        String content = "content";
        String content2 = "content2";
        String encryptedTitle = encryptData(title, sessionKey);
        String encryptedContent = encryptData(content, sessionKey);
        dataPeer.add(encryptedTitle, encryptedContent);

        String newContent = encryptData(content2, sessionKey);
        dataPeer.edit(encryptedTitle, newContent);

        Article article = gson.fromJson(decryptData(dataPeer.get(encryptedTitle), sessionKey), Article.class);
        assertEquals(article.text, content2);
    }

    @Test
    public void testHistory() throws GeneralSecurityException, RemoteException {
        String title = "title4";
        String content = "content";
        String content2 = "content2";
        String encryptedTitle = encryptData(title, sessionKey);
        String encryptedContent = encryptData(content, sessionKey);
        dataPeer.add(encryptedTitle, encryptedContent);
        String newContent = encryptData(content2, sessionKey);
        dataPeer.edit(encryptedTitle, newContent);

        String historyJson = decryptData(dataPeer.showHistory(encryptedTitle), sessionKey);
        History history = gson.fromJson(historyJson, History.class);

        assertEquals(2, history.getHistory().size());
        assertTrue(history.getHistory().containsValue("content"));
        assertTrue(history.getHistory().containsValue("content2"));
    }
}

// rkouidan
package com.vub.pdproject;

import com.google.gson.Gson;
import com.vub.pdproject.data.models.Article;
import com.vub.pdproject.protocols.DataPeerProtocol;
import com.vub.pdproject.utils.SecurityUtil;

import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.security.*;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

import static com.vub.pdproject.utils.SecurityUtil.*;

/**
 * The data peer stores the articles together with all changes applied to an article
 */
public class DataPeer implements DataPeerProtocol {
    // Holds all the articles
    private final ConcurrentHashMap<String, Article> articles = new ConcurrentHashMap<>();
    // Holds the history of each article
    private final ConcurrentHashMap<String, History> changes = new ConcurrentHashMap<>();
    // Capacity of the data-peer
    private final int capacity;
    private PublicKey publicKey;
    private PrivateKey privateKey;
    private SecretKey sessionKey;

    public DataPeer(int capacity) throws NoSuchAlgorithmException {
        this.capacity = capacity;
        KeyPair keyPair = SecurityUtil.generateKeyPair();
        publicKey = keyPair.getPublic();
        privateKey = keyPair.getPrivate();
    }

    /**
     * Returns the public key of the data-peer
     */
    @Override
    public PublicKey getKey() throws RemoteException {
        return publicKey;
    }

    /**
     * Method to exchange the session key with Wikipedia server
     * @param encryptedSessionKey
     */
    @Override
    public void setSessionKey(String encryptedSessionKey) throws GeneralSecurityException {
        byte[] decodedKey = stringToByte(encryptedSessionKey);
        byte[] decryptedKey = decryptKey(decodedKey, privateKey);
        this.sessionKey = new SecretKeySpec(decryptedKey,"AES");
    }

    /**
     * Ensures that the data-peer can no longer get remote calls
     */
    @Override
    public void exit() throws RemoteException {
        UnicastRemoteObject.unexportObject(this, true);
    }

    /**
     * Returns the number of articles in the data-peer
     */
    @Override
    public String getLoad() throws RemoteException, GeneralSecurityException {
        return encryptData(String.valueOf(articles.size()), sessionKey);
    }

    /**
     * Returns the capacity of a data-peer
     */
    @Override
    public String getCapacity() throws RemoteException, GeneralSecurityException {
        return encryptData(String.valueOf(this.capacity), sessionKey);
    }

    /**
     * Returns encrypted true if the data-peer is full else false
     */
    @Override
    public String isFull() throws GeneralSecurityException {
        return encryptData(String.valueOf(articles.size() == this.capacity), sessionKey);
    }

    /**
     * Returns the encrypted json article
     * @param title Title of the article
     */
    @Override
    public String get(String title) throws RemoteException, GeneralSecurityException {
        Gson gson = new Gson();
        String decryptedTitle = decryptData(title, sessionKey);
        return encryptData(gson.toJson(articles.get(decryptedTitle)), sessionKey);
    }

    /**
     * Adds an article to the peer
     * @param title The title of the article
     * @param content The content of the article
     */
    @Override
    public synchronized void add(String title, String content) throws GeneralSecurityException, RemoteException {
        // Decrypt information
        String decryptedTitle = decryptData(title, sessionKey);
        String decryptedContent = decryptData(content, sessionKey);
        if (articles.size() >= capacity) {
            throw new RemoteException("Capacity exceeded");
        } else {
            Article article = new Article();
            History history = new History();
            article.id = UUID.randomUUID().toString();
            article.title = decryptedTitle;
            article.text = decryptedContent;
            history.addEntry(decryptedContent);
            articles.put(decryptedTitle, article);
            changes.put(decryptedTitle, history);
        }
    }

    /**
     * Edit the content of an article
     * @param title The title of an article
     * @param content The new content of the article
     */
    @Override
    public synchronized void edit(String title, String content) throws RemoteException, GeneralSecurityException {
        // Decrypt information
        String decryptedTitle = decryptData(title, sessionKey);
        String decryptedContent = decryptData(content, sessionKey);
        Article article = articles.get(decryptedTitle);
        if (article != null) {
            History history = changes.get(decryptedTitle);
            article.text = decryptedContent;
            history.addEntry(decryptedContent);
        }
    }

    /**
     * Returns the encrypted json history of an article
     * @param title The title of the article
     */
    @Override
    public String showHistory(String title) throws RemoteException, GeneralSecurityException {
        Gson gson = new Gson();
        String decryptedTitle = decryptData(title, sessionKey);
        return encryptData(gson.toJson(changes.get(decryptedTitle)), sessionKey);
    }
}

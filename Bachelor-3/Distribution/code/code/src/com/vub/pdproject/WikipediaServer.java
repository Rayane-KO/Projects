// rkouidan
package com.vub.pdproject;

import java.io.IOException;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.security.*;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.PriorityBlockingQueue;

import com.google.gson.Gson;
import com.vub.pdproject.data.models.Article;
import com.vub.pdproject.protocols.ClientProtocol;
import com.vub.pdproject.protocols.DataPeerProtocol;
import com.vub.pdproject.protocols.UserDatabaseProtocol;
import com.vub.pdproject.protocols.WikipediaServerProtocol;
import com.vub.pdproject.utils.SecurityUtil;
import org.mindrot.jbcrypt.BCrypt;

import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;

import static com.vub.pdproject.utils.SecurityUtil.*;

/**
 * WikipediaServer interacts with clients and communicates with the data peers and user database server.
 * It is responsible for managing users and maintaining an index of the location of articles in data peers.
 */
public class WikipediaServer implements WikipediaServerProtocol {
    // User database
    private UserDatabaseProtocol userDatabase;
    // A priority queue with the data-peers, the smallest loaded peer is in the first position
    private final PriorityBlockingQueue<DataPeerProtocol> dataPeers;
    // Mutually exclusive lock for editing articles
    private final ConcurrentHashMap<String, String> editLocks;
    // Holds in which data-peer an article is
    private final ConcurrentHashMap<String, DataPeerProtocol> indexes;
    // Manages the session keys of the user database and data-peers
    private final ConcurrentHashMap<String, SecretKey> sessionKeys;
    private final PublicKey publicKey;
    private final PrivateKey privateKey;

    public WikipediaServer(List<Integer> dataPeersCapacities) throws NoSuchAlgorithmException, IOException {
        dataPeers = new PriorityBlockingQueue<>(dataPeersCapacities.size(), peerComparator);
        editLocks = new ConcurrentHashMap<>();
        indexes = new ConcurrentHashMap<>();
        sessionKeys = new ConcurrentHashMap<>();
        KeyPair keyPair = SecurityUtil.generateKeyPair();
        this.publicKey = keyPair.getPublic();
        this.privateKey = keyPair.getPrivate();
    }

    public Comparator<DataPeerProtocol> peerComparator = new Comparator<DataPeerProtocol>() {
        @Override
        public int compare(DataPeerProtocol p1, DataPeerProtocol p2) {
            try {
                int load1, load2, capacity1, capacity2;
                synchronized (sessionKeys) {
                    load1 = Integer.parseInt(decryptData(p1.getLoad(), sessionKeys.get(p1.toString())));
                    load2 = Integer.parseInt(decryptData(p2.getLoad(), sessionKeys.get(p2.toString())));
                    capacity1 = Integer.parseInt(decryptData(p1.getCapacity(), sessionKeys.get(p1.toString())));
                    capacity2 = Integer.parseInt(decryptData(p2.getCapacity(), sessionKeys.get(p2.toString())));
                }
                boolean thisFull = load1 == capacity1;
                boolean otherFull = load2 == capacity2;
                // if a peer is full it should not be proposed as smallest loaded peer
                if (thisFull && !otherFull) {
                    return 1;
                } else if (!thisFull && otherFull) {
                    return -1;
                } else {
                    return Integer.compare(load1, load2);
                }

            } catch (GeneralSecurityException | RemoteException e) {
                throw new RuntimeException(e);
            }
        }
    };

    public void setUserDatabase(UserDatabaseProtocol userDatabase) throws GeneralSecurityException, RemoteException {
        this.userDatabase = userDatabase;
        SecretKey userDatabaseSessionKey = saveKeys("userDatabase");
        byte[] encryptedKey = encryptKey(userDatabaseSessionKey.getEncoded(), userDatabase.getKey());
        userDatabase.setSessionKey(byteToString(encryptedKey));
    }

    public void addPeer(DataPeerProtocol dataPeer) throws GeneralSecurityException, RemoteException {
        SecretKey peerSessionKey = saveKeys(dataPeer.toString());
        byte[] encryptedPeerKey = encryptKey(peerSessionKey.getEncoded(), dataPeer.getKey());
        dataPeer.setSessionKey(byteToString(encryptedPeerKey));
        dataPeers.add(dataPeer);
    }

    public SecretKey saveKeys(String index) throws NoSuchAlgorithmException {
        SecretKey sessionKey = generateSessionKey();
        sessionKeys.put(index, sessionKey);
        return sessionKey;
    }

    /**
     * Returns the smallest loaded peer
     * @return
     */
    private DataPeerProtocol getSmallestLoadedPeer() {
        return dataPeers.peek();
    }

    /**
     * Hashes a password using the BCrypt algorithm
     * @param password The password to hash
     * found on: https://www.baeldung.com/java-password-hashing
     */
    private String hashPassword(String password){
        return BCrypt.hashpw(password, BCrypt.gensalt());
    }

    /**
     * Checks if a hashed password and the plain password are the same
     * @param plainPassword The password as plain-text
     * @param hashedPassword The hashed password
     */
    private boolean checkPassword(String plainPassword, String hashedPassword) {
        return BCrypt.checkpw(plainPassword, hashedPassword);
    }

    /**
     * Sens a message to the client
     * If the session key is available, it will use it
     * Otherwise, it will use the public key
     * @param message The message to send to the client
     * @param client The client that receives the message
     */
    public void sendMessageToClient(String message, ClientProtocol client) throws GeneralSecurityException, RemoteException {
        if (client != null) {
            SecretKey sessionKey = sessionKeys.get(client.toString());
            String encryptedMessage;
            if (sessionKey == null){
                encryptedMessage = encryptData(message, client.getKey());
            } else {
                encryptedMessage = encryptData(message, sessionKeys.get(client.toString()));
            }
            client.getMessage(encryptedMessage);
        }
    }

    /**
     * The next four methods are for testing purposes only!
     */
    public SecretKey getDatabaseKey(){
        return sessionKeys.get("userDatabase");
    }
    public SecretKey getPeerKey(DataPeerProtocol peer) {
        return sessionKeys.get(peer.toString());
    }
    public UserDatabaseProtocol getUserDatabase() { return userDatabase; }
    public PriorityBlockingQueue<DataPeerProtocol> getDataPeers() { return dataPeers; }

    /**
     * Returns the public key of the server
     */
    @Override
    public PublicKey getKey() throws RemoteException {
        return publicKey;
    }

    /**
     * Method to exchange the session key with clients
     * @param encryptedSessionKey The encrypted session key
     * @param client The client to exchange with
     */
    @Override
    public void setSessionKey(String encryptedSessionKey, ClientProtocol client) throws GeneralSecurityException {
        byte[] decodedKey = stringToByte(encryptedSessionKey);
        byte[]  decryptedKey = SecurityUtil.decryptKey(decodedKey, privateKey);
        this.sessionKeys.put(client.toString(), new SecretKeySpec(decryptedKey,"AES"));
    }

    /**
     * Registers a user in the database
     * @param username The username of the user
     * @param password The password of the user
     * @param client The client who wants to register
     */
    @Override
    public synchronized void register(String username, String password, ClientProtocol client) throws RemoteException, GeneralSecurityException {
        try {
            // Decrypt information
            String decryptedUsername = decryptData(username, privateKey);
            String decryptedPassword = decryptData(password, privateKey);
            // Hash the password
            String hashedPassword = hashPassword(decryptedPassword);
            SecretKey databaseSessionKey =  sessionKeys.get("userDatabase");
            // Encrypt to send to database
            String encryptedUsername = encryptData(decryptedUsername, databaseSessionKey);
            String encryptedPassword = encryptData(hashedPassword, databaseSessionKey);
            // Register the user in the database
            boolean success = Boolean.parseBoolean(decryptData(userDatabase.register(encryptedUsername, encryptedPassword), databaseSessionKey));
            String message;
            if (success){
                message = "You successfully registered, " + decryptedUsername;
            } else {
                message = "Username " + decryptedUsername + " is already taken!";
            }
            sendMessageToClient(message, client);
        } catch (RemoteException | GeneralSecurityException e) {
            sendMessageToClient("ERROR: Could not register the user, try again later.", client);
        }
    }

    /**
     * Logs a user in
     * @param username The username of the user
     * @param password The password of the user
     * @param client The client who wants to log in
     */
    @Override
    public String login(String username, String password, ClientProtocol client) throws RemoteException, GeneralSecurityException {
        boolean correct = false;
        SecretKey sessionKey = sessionKeys.get(client.toString());
        try {
            // Decrypt information
            String decryptedUsername = decryptData(username, sessionKey);
            String decryptedPassword = decryptData(password, sessionKey);
            // Encrypt information to send to database
            SecretKey databaseSessionKey = sessionKeys.get("userDatabase");
            String encryptedUsername = encryptData(decryptedUsername, databaseSessionKey);
            // Decrypt the answer from database
            String realPassword = decryptData(userDatabase.retrieve(encryptedUsername), databaseSessionKey);
            String message;
            if (realPassword == null){
                // User is not register
                message = "Please register first!";
            } else if (checkPassword(decryptedPassword, realPassword)){
                message = "Welcome " + decryptedUsername;
                correct = true;
            } else {
                message = "Username or password are incorrect!";
            }
            sendMessageToClient(message, client);
            // Encrypt the response
        } catch (RemoteException | GeneralSecurityException e) {
            sendMessageToClient("ERROR: Could not login the user, try again later.", client);
        }
        return encryptData(String.valueOf(correct), sessionKey);
    }

    /**
     * Gets an article back from the data-peers
     * @param title The title of the article
     * @param client The client who sends the request
     */
    @Override
    public boolean get(String title, ClientProtocol client) throws RemoteException, GeneralSecurityException {
        try {
            // Decrypt title
            SecretKey sessionKey = sessionKeys.get(client.toString());
            String decryptedTitle = decryptData(title, sessionKey);
            // Get data-peer in which article is stored
            DataPeerProtocol peer = indexes.get(decryptedTitle);
            if (peer != null){
                Gson gson = new Gson();
                // Encrypt information to send to data-peer
                String encryptedTitle = encryptData(decryptedTitle, sessionKeys.get(peer.toString()));
                // Translate the decrypted response back to an article
                Article article = gson.fromJson(decryptData(peer.get(encryptedTitle), sessionKeys.get(peer.toString())), Article.class);
                String content = article.text;
                sendMessageToClient(content, client);
                return true;
            } else {
                String message = "Article not found!";
                sendMessageToClient(message, client);
                return false;
            }
        } catch (RemoteException | GeneralSecurityException e) {
            sendMessageToClient("ERROR: Could not retrieve the article, try again later!", client);
            return false;
        }
    }

    /**
     * Adds an article to a data-peer
     * @param title The title of the article
     * @param content The content of the article
     * @param client The client who sends the request
     */
    @Override
    public synchronized void add(String title, String content, ClientProtocol client) throws RemoteException, GeneralSecurityException {
        // Decrypt the information
        SecretKey sessionKey = sessionKeys.get(client.toString());
        try {
            String decryptedTitle = decryptData(title, sessionKey);
            String decryptedContent = decryptData(content, sessionKey);
            // Get the smallest loaded peer and decrypt the response from the peer
            String message;
            DataPeerProtocol smallestLoadedPeer = getSmallestLoadedPeer();
            SecretKey peerKey = sessionKeys.get(smallestLoadedPeer.toString());
            String encryptedTitle = encryptData(decryptedTitle, peerKey);
            Gson gson = new Gson();
            Article article = null;
            for(DataPeerProtocol dataPeer: dataPeers){
                SecretKey key = sessionKeys.get(dataPeer.toString());
                String encryptedTitleSearch = encryptData(decryptedTitle, key);
                article = gson.fromJson(decryptData(dataPeer.get(encryptedTitleSearch), key), Article.class);
                if (article != null){
                    break;
                }
            }
            boolean dataPeerIsFull = Boolean.parseBoolean(decryptData(smallestLoadedPeer.isFull(), peerKey));
            if (dataPeerIsFull) {
                message = "No available data peer!";
            } else if (article != null) {
                message = "An article with this title already exists!";
            } else {
                // Encrypt the information to send to the data-peer
                String encryptedContent = encryptData(decryptedContent, peerKey);
                smallestLoadedPeer.add(encryptedTitle, encryptedContent);
                indexes.put(decryptedTitle, smallestLoadedPeer);
                message = "Article added successfully.";
                // re-add the peer to put it at the right position
                dataPeers.remove(smallestLoadedPeer);
                dataPeers.add(smallestLoadedPeer);
            }
            sendMessageToClient(message, client);
        } catch (RemoteException | GeneralSecurityException e) {
            sendMessageToClient("ERROR: Could not add article, try again later.", client);
        }
    }

    /**
     * Starts an edit session for an article
     * @param title The title of the article
     * @param client The client that sends the request
     */
    @Override
    public synchronized String startEdit(String title, ClientProtocol client) throws RemoteException, GeneralSecurityException {
        // Decrypt information
        SecretKey sessionKey = sessionKeys.get(client.toString());
        boolean success = false;
        try {
            String decryptedTitle = decryptData(title, sessionKey);
            if (editLocks.get(decryptedTitle) == null && get(title, client)) {
                editLocks.put(decryptedTitle, client.toString());
                String message = "You successfully started an edit session for '" + decryptedTitle + "'!";
                sendMessageToClient(message, client);
                success = true;
            } else if (editLocks.get(decryptedTitle) != null) {
                String message = "Article with title '" + decryptedTitle + "' is already in edit mode!";
                sendMessageToClient(message, client);
            } else {
                String message = "Article not found!";
                sendMessageToClient(message, client);
            }
        } catch (RemoteException | GeneralSecurityException e) {
            sendMessageToClient("ERROR: Could not start a new edit session for this article, try again later.", client);
        }
        return encryptData(String.valueOf(success), sessionKey);
    }

    /**
     * Edit the content of an article
     * @param title The title of the article
     * @param content The new content of the article
     * @param client The client that sends the request
     */
    @Override
    public synchronized void edit(String title, String content, ClientProtocol client) throws RemoteException, GeneralSecurityException {
        // Decrypt information
        SecretKey sessionKey = sessionKeys.get(client.toString());
        try {
            String decryptedTitle = decryptData(title, sessionKey);
            String decryptedContent = decryptData(content, sessionKey);
            // Ensure that the client can edit this article
            if (editLocks.get(decryptedTitle) != null){
                if (editLocks.get(decryptedTitle).equals(client.toString())){
                    DataPeerProtocol peer = indexes.get(decryptedTitle);
                    if (peer != null){
                        // Encrypt the information to send to the data-peer
                        SecretKey peerKey = sessionKeys.get(peer.toString());
                        String encryptedTitle = encryptData(decryptedTitle, peerKey);
                        String encryptedContent = encryptData(decryptedContent, peerKey);
                        peer.edit(encryptedTitle, encryptedContent);
                        String message = "Article edited successfully!";
                        sendMessageToClient(message, client);
                    } else {
                        String message = "ERROR: Article not found!";
                        sendMessageToClient(message, client);
                    }
                } else {
                    String message = "You can not edit this article because it is in edit mode!";
                    sendMessageToClient(message, client);
                }
            } else {
                String message = "You are not in an edit session!";
                sendMessageToClient(message, client);
            }
        } catch (RemoteException | GeneralSecurityException e) {
            sendMessageToClient("ERROR: Could not edit this article, try again later.", client);
        }
    }

    /**
     * Ends an edit session for an article
     * @param title The title of the article
     * @param client The client that sends the request
     */
    @Override
    public synchronized void endEdit(String title, ClientProtocol client) throws RemoteException, GeneralSecurityException {
        SecretKey sessionKey = sessionKeys.get(client.toString());
        try {
            String decryptedTitle = decryptData(title, sessionKey);
            String editLock = editLocks.get(decryptedTitle);
            if (editLock != null){
                if (editLocks.get(decryptedTitle).equals(client.toString())){
                    editLocks.remove(decryptedTitle);
                    String message = "Edit session successfully closed!";
                    sendMessageToClient(message, client);
                } else {
                    String message = "You did not start an edit session for this article!";
                    sendMessageToClient(message, client);
                }
            } else {
                String message = "Edit session not found!";
                sendMessageToClient(message, client);
            }
        } catch (RemoteException | GeneralSecurityException e) {
            sendMessageToClient("ERROR: Could not end the edit session for this article, try again later.", client);
        }
    }

    /**
     * Shows the history of changes of an article
     * @param title The title of the article
     * @param client The client that sends the request
     */
    @Override
    public void showHistory(String title, ClientProtocol client) throws RemoteException, GeneralSecurityException {
        Gson gson = new Gson();
        // Decrypt information
        SecretKey sessionKey = sessionKeys.get(client.toString());
        try {
            String decryptedTitle = decryptData(title, sessionKey);
            DataPeerProtocol peer = indexes.get(decryptedTitle);
            // Encrypt the data to send to the data-peer
            String encryptedTitle = encryptData(decryptedTitle, sessionKeys.get(peer.toString()));
            // Translate the response back to a history
            History history = gson.fromJson(decryptData(peer.showHistory(encryptedTitle), sessionKeys.get(peer.toString())), History.class);
            if (history != null) {
                // Show the changes to the client
                for (ConcurrentHashMap.Entry<Integer, String> entry : history.getHistory().entrySet()) {
                    int version = entry.getKey();
                    String content = entry.getValue();
                    String message = "v" + version + ": " + content;
                    sendMessageToClient(message, client);
                }
            } else {
                sendMessageToClient("History not found!", client);
            }
        } catch (RemoteException | GeneralSecurityException e) {
            sendMessageToClient("ERROR: Could not show the history of this article, try again later.", client);
        }
    }

    /**
     * Ensures that the components can no longer get remote calls
     */
    public void exit() throws RemoteException {
        try {
            for(DataPeerProtocol peer: dataPeers){
                peer.exit();
            }
            UnicastRemoteObject.unexportObject(this, true);
            userDatabase.exit();
        } catch (RemoteException e) {
            System.err.println("Could not exit the client! - " + e);
        }
    }
}

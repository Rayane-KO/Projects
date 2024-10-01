// rkouidan
package com.vub.pdproject;

import com.vub.pdproject.protocols.ClientProtocol;
import com.vub.pdproject.protocols.WikipediaServerProtocol;

import javax.crypto.SecretKey;
import javax.security.auth.DestroyFailedException;
import java.io.Serializable;
import java.rmi.NoSuchObjectException;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.security.*;
import java.util.Scanner;

import static com.vub.pdproject.utils.SecurityUtil.*;

/**
 * Client is the component a user can interact with to use the Wikipedia storage system
 */
public class Client implements ClientProtocol, Serializable {
    // Wikipedia server
    private WikipediaServerProtocol wikipediaServer;
    private boolean loggedIn = false;
    private final PublicKey publicKey;
    private final PrivateKey privateKey;
    private SecretKey sessionKey = null;

    public Client() throws NoSuchAlgorithmException {
        KeyPair keyPair = generateKeyPair();
        publicKey = keyPair.getPublic();
        privateKey = keyPair.getPrivate();
    }

    public void setWikipediaServer(WikipediaServerProtocol wikipediaServer) {
        this.wikipediaServer = wikipediaServer;
    }

    /**
     * Prints a message for the client
     * @param message Message to send (encrypted)
     */
    public void getMessage(String message) throws GeneralSecurityException {
        String decryptedMessage;
        if (sessionKey == null){
            decryptedMessage = decryptData(message, privateKey);
        } else {
            decryptedMessage = decryptData(message, sessionKey);
        }
        System.out.println(decryptedMessage);
    }

    /**
     * Exchange the session key between client and server
     */
    public void exchangeKeys() {
        try {
            sessionKey = generateSessionKey();
            byte[] encryptedKey = encryptKey(sessionKey.getEncoded(), wikipediaServer.getKey());
            // exchange the encrypted session key
            wikipediaServer.setSessionKey(byteToString(encryptedKey), this);
        } catch (GeneralSecurityException | RemoteException e) {
            errorMessage("Security error occurred. Try again later...");
            throw new RuntimeException(e);
        }
    }

    private void errorMessage(String message) {
        System.err.println(message);
    }

    /**
     * Returns the public key of the client
     */
    @Override
    public PublicKey getKey() {
        return publicKey;
    }

    /**
     * Checks if a user is logged in (for testing purposes)
     */
    public boolean getStatus() {
        return loggedIn;
    }

    /**
     * Registers an account for a new user
     * @param username A unique username
     * @param password Password for the new account
     */
    @Override
    public void register(String username, String password) throws GeneralSecurityException, RemoteException {
        String encryptedUsername = encryptData(username, wikipediaServer.getKey());
        String encryptedPassword = encryptData(password, wikipediaServer.getKey());
        // send the encrypted username and password
        wikipediaServer.register(encryptedUsername, encryptedPassword, this);
    }

    /**
     * Asks the server to log in a user with an existing account
     * @param username The username of the user
     * @param password The corresponding password
     */
    @Override
    public void login(String username, String password) throws RemoteException, GeneralSecurityException {
        exchangeKeys();
        String encryptedUsername = encryptData(username, sessionKey);
        String encryptedPassword = encryptData(password, sessionKey);
        String encryptedResult = wikipediaServer.login(encryptedUsername, encryptedPassword, this);
        loggedIn = Boolean.parseBoolean(decryptData(encryptedResult, sessionKey));
    }

    /**
     * Asks the server for the content of an article
     * @param title The title of the article to search
     */
    @Override
    public void get(String title) throws RemoteException, GeneralSecurityException {
        String encryptedTitle = encryptData(title, sessionKey);
        wikipediaServer.get(encryptedTitle, this);
    }

    /**
     * Asks the server to add an article to the system
     * @param title The title of the article
     * @param content The content of the article
     */
    @Override
    public void add(String title, String content) throws RemoteException, GeneralSecurityException {
        String encryptedTitle = encryptData(title, sessionKey);
        String encryptedContent = encryptData(content, sessionKey);
        wikipediaServer.add(encryptedTitle, encryptedContent, this);
    }

    /**
     * Asks the server to start an edit session for an article
     * @param title The title of the article to edit
     * @return A boolean that says if the edit session started or not
     */
    @Override
    public boolean startEdit(String title) throws RemoteException, GeneralSecurityException {
        String encryptedTitle = encryptData(title, sessionKey);
        return Boolean.parseBoolean(decryptData(wikipediaServer.startEdit(encryptedTitle, this), sessionKey));
    }

    /**
     * Asks the server to edit an article
     * @param title The title of the article to edit
     * @param content The new content of the article
     */
    @Override
    public void edit(String title, String content) throws RemoteException, GeneralSecurityException {
        String encryptedTitle = encryptData(title, sessionKey);
        String encryptedContent = encryptData(content, sessionKey);
        wikipediaServer.edit(encryptedTitle, encryptedContent, this);
    }

    /**
     * Asks the server to end the edit session of an article
     * @param title The title of the article
     */
    @Override
    public void endEdit(String title) throws RemoteException, GeneralSecurityException {
        String encryptedTitle = encryptData(title, sessionKey);
        wikipediaServer.endEdit(encryptedTitle, this);
    }

    /**
     * Asks the server for the history of changes of an article
     * @param title The title of the article
     */
    @Override
    public void showHistory(String title) throws RemoteException, GeneralSecurityException {
        String encryptedTitle = encryptData(title, sessionKey);
        wikipediaServer.showHistory(encryptedTitle, this);
    }

    /**
     * Ends the client session by destroying sensitive information and
     * ensures that the client can no longer receive remote calls
     */
    public void exit() throws NoSuchObjectException, DestroyFailedException {
        privateKey.destroy();
        sessionKey.destroy();
        loggedIn = false;
        UnicastRemoteObject.unexportObject(this, true);
    }
    /**
     * Runs the commands of the user
     */
    public void run() throws RemoteException {
        Scanner scanner = new Scanner(System.in);
        boolean editing = false; // Checks if the user is in edit mode
        boolean exit = false;    // Users wants to exit
        String editTitle = null; // Title of the article in edit mode

        while (!exit) {
            if (editing) {
                // Choices for logged-in users
                System.out.println("Choose an action: (1) Edit article (2) See edit history (3) Leave edit session");
            } else if (this.loggedIn) {
                // Choices for users in edit mode
                System.out.println("Choose an action: (1) Get article (2) Add article (3) Start edit session (4) Exit");
            } else {
                // Choices for users that are not logged-in
                System.out.println("Choose an action: (1) Register (2) Login (3) Exit");
            }

            // Check that the choice is a number
            if (scanner.hasNextInt()) {
                int choice = scanner.nextInt();
                scanner.nextLine();

                try {
                    if (!this.loggedIn) {
                        // Commands for users that are not logged-in
                        switch (choice) {
                            case 1:
                                System.out.print("Enter username: ");
                                String username = scanner.nextLine();
                                System.out.print("Enter password: ");
                                String password = scanner.nextLine();
                                this.register(username, password);
                                break;
                            case 2:
                                System.out.print("Enter username: ");
                                String loginUsername = scanner.nextLine();
                                System.out.print("Enter password: ");
                                String loginPassword = scanner.nextLine();
                                this.login(loginUsername, loginPassword);
                                break;
                            case 3:
                                System.out.println("Exiting...");
                                scanner.close();
                                this.exit();
                                System.exit(0);
                                return;
                            default:
                                System.out.println("Invalid choice. Please try again.");
                        }
                    } else if (editing) {
                        // Commands for users in edit mode
                        switch (choice) {
                            case 1:
                                System.out.print("Enter new content: ");
                                String editContent = scanner.nextLine();
                                this.edit(editTitle, editContent);
                                break;
                            case 2:
                                this.showHistory(editTitle);
                                break;
                            case 3:
                                this.endEdit(editTitle);
                                editing = false;
                                break;
                            default:
                                System.out.println("Invalid choice. Please try again.");
                        }
                    } else {
                        switch (choice) {
                            // Commands for logged-in users
                            case 1:
                                System.out.print("Enter the title of the article: ");
                                String title = scanner.nextLine();
                                this.get(title);
                                break;
                            case 2:
                                System.out.print("Enter the title of the article: ");
                                String newTitle = scanner.nextLine();
                                System.out.print("Enter the content of the article: ");
                                String newContent = scanner.nextLine();
                                this.add(newTitle, newContent);
                                break;
                            case 3:
                                System.out.print("Enter title: ");
                                editTitle = scanner.nextLine();
                                editing = this.startEdit(editTitle);
                                break;
                            case 4:
                                System.out.println("Exiting...");
                                exit = true;
                                scanner.close();
                                UnicastRemoteObject.unexportObject(this, true);
                                System.exit(0);
                                return;
                            default:
                                System.out.println("Invalid choice. Please try again.");
                        }
                    }
                } catch (RemoteException | GeneralSecurityException | DestroyFailedException e) {
                    System.err.println("An error occurred. Try again later...");
                }
            } else {
                    scanner.next();
                    System.out.println("Invalid input. Please enter a number.");
                }
            }
        }
}

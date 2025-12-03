package info.kgeorgiy.ja.mochekov.bank;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface Bank extends Remote {
    /**
     * Creates a new account with specified identifier if it does not already exist.
     * @param id account id
     * @return created or existing account.
     */
    Account createAccount(String id) throws RemoteException;

    /**
     * Returns account by identifier.
     * @param id account id
     * @return account with specified identifier or {@code null} if such account does not exist.
     */
    Account getAccount(String id) throws RemoteException;

    /**
     * Creates a new person with specified data.
     * @param firstName person's first name
     * @param lastName person's last name
     * @param passportId person's passportId number
     * @return created or existing person.
     */
    Person createPerson(String firstName, String lastName, String passportId) throws RemoteException;

    /**
     * Returns a remote person by passportId number.
     * @param passportId person's passportId number
     * @return person or null if no such person exists
     */
    Person getRemotePerson(String passportId) throws RemoteException;

    /**
     * Returns a local person by passportId number.
     * @param passportId person's passportId number
     * @return local person or null if no such person exists
     */
    LocalPerson getLocalPerson(String passportId) throws RemoteException;
}
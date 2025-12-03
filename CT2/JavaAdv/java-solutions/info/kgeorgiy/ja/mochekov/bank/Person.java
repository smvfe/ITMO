package info.kgeorgiy.ja.mochekov.bank;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.Set;

public interface Person extends Remote {
    /** Returns person's first name. */
    String getFirstName() throws RemoteException;

    /** Returns person's last name. */
    String getLastName() throws RemoteException;

    /** Returns person's passportId number. */
    String getpassportId() throws RemoteException;

    /** Creates a new account with specified sub-identifier if it doesn't already exist. */
    Account createAccount(String subId) throws RemoteException;

    /** Returns account by sub-identifier or null if not exists. */
    Account getAccount(String subId) throws RemoteException;

    /** Returns all accounts sub-identifiers belonging to this person. */
    Set<String> getAccounts() throws RemoteException;
}
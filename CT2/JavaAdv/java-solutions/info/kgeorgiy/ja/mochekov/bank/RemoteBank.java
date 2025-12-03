package info.kgeorgiy.ja.mochekov.bank;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

public class RemoteBank implements Bank {
    private final int port;
    private final ConcurrentMap<String, Account> accounts = new ConcurrentHashMap<>();
    private final ConcurrentMap<String, Person> persons = new ConcurrentHashMap<>();

    public RemoteBank(final int port) {
        this.port = port;
    }

    @Override
    public Account createAccount(final String id) throws RemoteException {
        System.out.println("Creating account " + id);
        final Account account = new RemoteAccount(id);
        if (accounts.putIfAbsent(id, account) == null) {
            UnicastRemoteObject.exportObject(account, port);
            return account;
        } else {
            return getAccount(id);
        }
    }

    @Override
    public Account getAccount(final String id) {
        System.out.println("Retrieving account " + id);
        return accounts.get(id);
    }

    @Override
    public Person createPerson(String firstName, String lastName, String passportId) throws RemoteException {
        System.out.println("Creating person with passportId " + passportId);

        Person person = getRemotePerson(passportId);
        if (person != null) {
            if (!firstName.equals(person.getFirstName()) || !lastName.equals(person.getLastName())) {
                throw new RemoteException("Person data mismatch");
            }
            return person;
        }

        RemotePerson newPerson = new RemotePerson(firstName, lastName, passportId, this);
        persons.put(passportId, newPerson);
        UnicastRemoteObject.exportObject(newPerson, port);
        return newPerson;
    }

    @Override
    public Person getRemotePerson(String passportId) {
        System.out.println("Getting remote person with passportId " + passportId);
        return persons.get(passportId);
    }

    @Override
    public LocalPerson getLocalPerson(String passportId) throws RemoteException {
        System.out.println("Getting local person with passportId " + passportId);
        Person remotePerson = getRemotePerson(passportId);
        if (remotePerson == null) {
            return null;
        }
        return new LocalPerson(remotePerson);
    }
}
package info.kgeorgiy.ja.mochekov.bank;

import java.rmi.RemoteException;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

public class RemotePerson extends AbstractPerson {
    private final Bank bank;
    private final ConcurrentMap<String, Account> accounts = new ConcurrentHashMap<>();

    public RemotePerson(String firstName, String lastName, String passportId, Bank bank) {
        super(firstName, lastName, passportId);
        this.bank = bank;
    }

    @Override
    public Account createAccount(String subId) throws RemoteException {
        String fullId = getpassportId() + ":" + subId;
        Account account = bank.createAccount(fullId);
        accounts.put(subId, account);
        return account;
    }

    @Override
    public Account getAccount(String subId) throws RemoteException {
        return bank.getAccount(getpassportId() + ":" + subId);
    }

    @Override
    public Set<String> getAccounts() {
        return accounts.keySet();
    }
}
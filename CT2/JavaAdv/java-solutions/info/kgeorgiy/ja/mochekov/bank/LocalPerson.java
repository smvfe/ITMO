package info.kgeorgiy.ja.mochekov.bank;

import java.io.Serializable;
import java.rmi.RemoteException;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

public class LocalPerson extends AbstractPerson implements Serializable {
    private final Map<String, LocalAccount> accounts = new ConcurrentHashMap<>();

    public LocalPerson(String firstName, String lastName, String passportId) {
        super(firstName, lastName, passportId);
    }

    public LocalPerson(Person remotePerson) throws RemoteException {
        super(remotePerson.getFirstName(), remotePerson.getLastName(), remotePerson.getpassportId());

        for (String subId : remotePerson.getAccounts()) {
            Account remoteAccount = remotePerson.getAccount(subId);
            if (remoteAccount != null) {
                accounts.put(subId, new LocalAccount(remoteAccount.getId(), remoteAccount.getAmount()));
            }
        }
    }

    @Override
    public Account createAccount(String subId) {
        LocalAccount account = new LocalAccount(getpassportId() + ":" + subId, 0);
        accounts.put(subId, account);
        return account;
    }

    @Override
    public Account getAccount(String subId) {
        return accounts.get(subId);
    }

    @Override
    public Set<String> getAccounts() {
        return accounts.keySet();
    }
}
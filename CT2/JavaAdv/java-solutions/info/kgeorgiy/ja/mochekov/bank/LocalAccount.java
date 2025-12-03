package info.kgeorgiy.ja.mochekov.bank;

import java.io.Serializable;

public class LocalAccount implements Account, Serializable {
    private final String id;
    private int amount;

    public LocalAccount(String id, int amount) {
        this.id = id;
        this.amount = amount;
    }

    @Override
    public String getId() {
        return id;
    }

    @Override
    public synchronized int getAmount() {
        return amount;
    }

    @Override
    public synchronized void setAmount(int amount) {
        this.amount = amount;
    }
}

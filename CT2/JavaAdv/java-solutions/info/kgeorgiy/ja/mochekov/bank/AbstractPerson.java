package info.kgeorgiy.ja.mochekov.bank;

import java.util.Objects;

public abstract class AbstractPerson implements Person {
    private final String firstName;
    private final String lastName;
    private final String passportId;

    public AbstractPerson(String firstName, String lastName, String passportId) {
        this.firstName = firstName;
        this.lastName = lastName;
        this.passportId = passportId;
    }

    @Override
    public String getFirstName() {
        return firstName;
    }

    @Override
    public String getLastName() {
        return lastName;
    }

    @Override
    public String getpassportId() {
        return passportId;
    }

    @Override
    public int hashCode() {
        return Objects.hash(passportId);
    }
}
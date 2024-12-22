import React, {useRef, useState} from 'react';

const Register = ({ createUser, users, setPage }) => {

    const loginInputRef = useRef(null)
    const nameInputRef = useRef(null)
    const passwordInputRef = useRef(null)
    const [error, setError] = useState('')

    const validateLogin = (login) => {
        const loginRegex = /^[a-z]{3,16}$/;
        return loginRegex.test(login) && !users.some(user => user.login === login);
    };

    const validateName = (name) => {
        return (name.length >= 1 && name.length <= 32) && !users.some(user => user.name === name);
    };

    const handleRegister = (event) => {
        event.preventDefault()
        const login = loginInputRef.current.value;
        const name = nameInputRef.current.value;

        if (!validateLogin(login)) {
            setError('Invalid login. Must be 3-16 latin lowercase letters and unique.');
            return;
        }
        if (!validateName(name)) {
            setError('Invalid name. Must be 1-32 characters and unique.');
            return;
        }
        createUser({
            login: login,
            name: name
        })
        setPage('enter');
    };

    return (
        <div className="enter form-box">
            <div className="header">Register</div>
            <div className="body">
                <form method="" action="" onSubmit={handleRegister}>
                    <input type="hidden" name="action" value="enter"/>
                    <div className="field">
                        <div className="name">
                            <label htmlFor="login">Name</label>
                        </div>
                        <div className="value">
                            <input
                                autoFocus
                                name="name"
                                ref={nameInputRef}
                                onChange={() => setError(null)}
                            />
                        </div>
                    </div>
                    <div className="field">
                        <div className="name">
                            <label htmlFor="login">Login</label>
                        </div>
                        <div className="value">
                            <input
                                name="login"
                                ref={loginInputRef}
                                onChange={() => setError(null)}
                            />
                        </div>
                    </div>
                    <div className="field">
                        <div className="name">
                            <label htmlFor="password">Password</label>
                        </div>
                        <div className="value">
                            <input
                                name="password"
                                type="password"
                                ref={passwordInputRef}
                                onChange={() => setError(null)}
                            />
                        </div>
                    </div>
                    {error
                        ? <div className={'error'}>{error}</div>
                        : null
                    }
                    <div className="button-field">
                        <input type="submit" value="Register"/>
                    </div>
                </form>
            </div>
        </div>
    );
};

export default Register;
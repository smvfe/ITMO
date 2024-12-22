import React from 'react';

const Navigation = ({user, setPage}) => {
    return (
        <nav>
            <ul>
                <li>
                    <a href="" onClick={(event) => {
                        event.preventDefault()
                        setPage('index')
                    }}>Home</a>
                </li>
                <li><a href="" onClick={(event) => {
                    event.preventDefault()
                    setPage('users')
                    }}>Users</a>
                </li>
                {user
                    ?
                    <li>
                        <a href="" onClick={(event)=>{
                            event.preventDefault()
                            setPage('writePost')
                        }}>
                            Write Post
                        </a>
                    </li>
                    : null
                }
                <li><a href="" onClick={(event) => {
                    event.preventDefault()
                    }}>
                        Posts
                    </a>
                </li>
            </ul>
        </nav>
    );
};

export default Navigation;
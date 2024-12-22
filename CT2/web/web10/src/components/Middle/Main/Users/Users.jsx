import React, {useMemo} from 'react';

const Users = ({ users }) => {

    const sortedUsers = useMemo(() => {
        if (!users)
            return []
        return users.sort((a, b) => b.id - a.id)
    }, [users])

    return (
        <div className="users datatable">
            <div className="caption">User List</div>
            <table>
                <thead>
                <tr>
                    <th>Id</th>
                    <th>Login</th>
                    <th>Name</th>
                </tr>
                </thead>
                <tbody>
                {users.length === 0 ? (
                    <tr className="noData">
                        <td colSpan="3">No data</td>
                    </tr>
                ) : (
                    sortedUsers.map((user) => (
                        <tr key={user.id}>
                            <td className="id">{user.id}</td>
                            <td className="login">{user.login}</td>
                            <td className="name">{user.name}</td>
                        </tr>
                    ))
                )}
                </tbody>
            </table>
        </div>
    );
};

export default Users;
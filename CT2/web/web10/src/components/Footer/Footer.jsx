import React from 'react';

const Footer = ({ usersNum, postsNum }) => {
    return (
        <footer>
            <a href="#">Codehorses</a> 2099 by Mike Mirzayanov
            <div>Users: {usersNum}</div>
            <div>Posts: {postsNum}</div>
        </footer>
    );
};

export default Footer;
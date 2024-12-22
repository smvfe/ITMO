import React from 'react';

const Section = ({post}) => {
    return (
        <section>
            <div className="header">
                {post.title}
            </div>
            <div className="body">
                {post.text}
            </div>
            <div className="footer">
                <a href="#">View all</a>
            </div>
        </section>
    );
};

export default Section;
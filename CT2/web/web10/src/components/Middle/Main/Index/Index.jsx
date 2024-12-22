import React from 'react';
import styles from './Index.module.css';
import voteUpImg from '../../../../assets/img/voteup.png';
import voteDownImg from '../../../../assets/img/votedown.png';
import dateImg from '../../../../assets/img/date_16x16.png';
import commentsImg from '../../../../assets/img/comments_16x16.png';
import paperclipImg from '../../../../assets/img/paperclip-16x16.png';

const Index = ({ posts = [] }) => {
    const sortedPosts = posts.sort((a, b) => b.id - a.id);

    return (
        <div>
            {sortedPosts.map((post) => (
                <article key={post.id} className={styles.article}>
                    <div className={styles.title}><a href={`#`}>{post.title}</a></div>
                    <div className={styles.information}>By smone, smtime</div>
                    <div className={styles.body}>{post.text}</div>
                    <div className={styles.tags}>
                        {post.tags && post.tags.map((tag, index) => (
                            <a href="#" key={index}>{tag.name}</a>
                        ))}
                    </div>
                    <ul className={styles.attachment}>
                        <li><img src={paperclipImg} alt="Attachment"/> Announcement of <a href="#">Codeforces Round #510
                            (Div. 1)</a></li>
                        <li><img src={paperclipImg} alt="Attachment"/> Announcement of <a href="#">Codeforces Round #510
                            (Div. 2)</a></li>
                    </ul>
                    <div className={styles.footer}>
                        <div className={styles.left}>
                            <img src={voteUpImg} title="Vote Up" alt="Vote Up"/>
                            <span className={styles.positiveScore}>+173</span>
                            <img src={voteDownImg} title="Vote Down" alt="Vote Down"/>
                        </div>
                        <div className={styles.right}>
                            <img src={dateImg} title="Publish Time" alt="Publish Time"/>
                            {post.creationTime}
                            <img src={commentsImg} title="Comments" alt="Comments"/>
                            <a href="#">{post.comments ? post.comments.length : 0}</a>
                        </div>
                    </div>
                </article>
            ))}
        </div>
    );
};

export default Index;
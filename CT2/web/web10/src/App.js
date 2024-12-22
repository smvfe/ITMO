import './App.css';
import Enter from "./components/Middle/Main/Enter/Enter";
import WritePost from "./components/Middle/Main/WritePost/WritePost";
import Index from "./components/Middle/Main/Index/Index";
import Register from "./components/Middle/Main/Register/Register";
import React, {useCallback, useState} from "react";
import Middle from "./components/Middle/Middle";
import Footer from "./components/Footer/Footer";
import Header from "./components/Header/Header";
import Users from "./components/Middle/Main/Users/Users";

function App({usersData, postsData}) {

    const [user, setUser] = useState(null)
    const [page, setPage] = useState('index')
    const [posts, setPosts] = useState(postsData)
    const [users, setUsers] = useState(usersData)

    const createPost = useCallback((post) => {
        const maxId = Math.max(...posts.map((post) => post.id)) + 1
        setPosts([...posts, { id: maxId, title: post.title, text: post.text}])
    }, [posts])

    const createUser = useCallback((user) => {
        const maxId = Math.max(...users.map((user) => user.id)) + 1
        setUsers([...users, { id: maxId, name: user.name, login: user.login }])
    }, [users])

    const getPage = useCallback((page) => {
        switch (page) {
            case 'index':
                return (<Index posts={posts}/>)
            case 'enter':
                return (<Enter users={users} setUser={setUser} setPage={setPage}/>)
            case 'writePost':
                return (<WritePost createPost={createPost} setPage={setPage}/>)
            case 'register':
                return (<Register createUser={createUser} users={usersData} setPage={setPage} />);
            case 'users':
                return (<Users users={users} />);
            default:
                return (<Index posts={posts}/>);
        }
    }, [users])

    return (
        <div className="App">
            <Header setUser={setUser} setPage={setPage} user={user}/>
            <Middle
                posts={posts}
                page={getPage(page)}
            />
            <Footer usersNum={usersData.length}  postsNum={postsData.length}/>
        </div>
    );
}

export default App;

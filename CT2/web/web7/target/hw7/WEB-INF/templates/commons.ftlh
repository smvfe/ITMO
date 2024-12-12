<#-- @ftlvariable name="user" type="ru.itmo.wp.model.domain.User" -->
<#-- @ftlvariable name="message" type="java.lang.String" -->
<#-- @ftlvariable name="userCount" type="java.lang.Long" -->
<#macro page>
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Codeforces</title>
    <link rel="stylesheet" type="text/css" href="css/normalize.css">
    <link rel="stylesheet" type="text/css" href="css/style.css">
    <link rel="stylesheet" type="text/css" href="css/form.css">
    <link rel="stylesheet" type="text/css" href="css/datatable.css">
    <script src="/js/jquery-3.6.1.min.js"></script>
    <script src="/js/notify.min.js"></script>
    <script src="/js/app.js"></script>
</head>
<body>
<header>
    <a href="/index"><img src="img/logo.png" alt="Codeforces" title="Codeforces"/></a>
    <div class="languages">
        <a href="#"><img src="img/gb.png" alt="In English" title="In English"/></a>
        <a href="#"><img src="img/ru.png" alt="In Russian" title="In Russian"/></a>
    </div>
    <div class="enter-or-register-box">
        <#if user??>
            ${user.login}
            |
            <a href="/logout">Logout</a>
        <#else>
            <a href="/enter">Enter</a>
            |
            <a href="/register">Register</a>
        </#if>
    </div>
    <nav>
        <ul>
            <li><a href="/index">Home</a></li>
            <li><a href="/users">Users</a></li>
            <#if user??>
                <li><a href="/article">Create article</a></li>
            </#if>
        </ul>
    </nav>
</header>
<div class="middle">
    <aside>
        <section>
            <div class="header">
                Pay attention
            </div>
            <div class="body">
                Lorem ipsum dolor sit amet, consectetur adipisicing elit. Cupiditate ducimus enim facere impedit nobis,
                nulla placeat quam suscipit unde voluptatibus.
            </div>
            <div class="footer">
                <a href="#">View all</a>
            </div>
        </section>
    </aside>
    <main>
        <#nested/>
    </main>
</div>
<footer>
    <p>Total registered users: ${userCount!}</p>
    <a href="/index">Codeforces</a> &copy; 2022 by Mike Mirzayanov
</footer>
<script>
    $(function () {
        <#if message??>
            notify("${message?js_string}");
        </#if>
    })
</script>
</body>
</html>
</#macro>

<#macro userlink user nameOnly=false>
    <#if nameOnly>
        <a href="/user?handle=${user.handle}">${user.name}</a>
    <#else>
        <a href="/user?handle=${user.handle}" class="colorful ${user.color?string?lower_case}">${user.handle}</a>
    </#if>
</#macro>

<#function findBy items key value>
    <#list items as item>
        <#if item[key] == value>
            <#return item/>
        </#if>
    </#list>
</#function>

<#macro findPost post>
    <article class = "post">
        <#assign creator = findBy(users, "id", post.userId)>
        <div class="title">${post.title}</div>
        <div class="information">
            <#if user?? && user.id == creator.id>
                By <@userlink creator/>
            <#else>
                By <@userlink creator/>
            </#if>
        </div>
        <#if post.text?length gt 350>
            <div class="body">${post.text?substring(0, 350)}...</div>
        <#else>
            <div class="body">${post.text}</div>
        </#if>
        <div class="footer">
            <div class="left">
                <img src="img/voteup.png" title="Vote Up" alt="Vote Up"/>
                <span class="positive-score">+173</span>
                <img src="img/votedown.png" title="Vote Down" alt="Vote Down"/>
            </div>
            <div class="right">
                <img src="img/date_16x16.png" title="Publish Time" alt="Publish Time"/>
                <p>2 days ago</p>
                <img src="img/comments_16x16.png" title="Comments" alt="Comments"/>
                <a href="#">68</a>
            </div>
        </div>
    </article>
</#macro>

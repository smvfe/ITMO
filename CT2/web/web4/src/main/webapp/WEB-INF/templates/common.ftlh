
<#macro page>
    <!DOCTYPE html>
    <html lang="ru">
    <head>
        <meta charset="UTF-8">
        <title>Codeforces</title>
        <link rel="stylesheet" href="/css/normalize.css">
        <link rel="stylesheet" href="/css/style.css">
        <link rel="icon" href="/favicon.ico">
    </head>
    <body>
    <@header/>
    <div class="middle">
        <@sidebar/>
        <main>
            <#nested/>
        </main>
    </div>
    <@footer/>
    </body>
    </html>
</#macro>

<#macro header>
    <header>
        <a href="#" class="logo"><img alt="Codeforces" title="Codeforces"
                                      src="/img/codeforces-logo-with-telegram.png"></a>
        <div class="languages">
            <a href="#"><img alt="In English" title="In English"
                             src="/img/gb.png"></a>
            <a href="#"><img alt="In Russian" title="In Russian"
                             src="/img/ru.png"></a>
        </div>
        <div class="enter-or-register-box">
            <#if user??>
                <@userlink user true/>
                |
                <a href="/logout">Logout</a>
            <#else>
                <a href="/enter">Enter</a>
                |
                <a href="/register">Register</a>
            </#if>
        </div>
        <nav>
            <#assign items=[
            {"href": "/index", "name": "Home"},
            {"href": "/users", "name": "Users"},
            {"href": "/misc/help", "name": "Help"},
            {"href": "/contests", "name": "Contests"}
            ]/>
            <ul>
                <#list items as item>
                    <li>
                        <li<#if item.href == currentPage> class="active"</#if>>
                        <a href="${item.href}">${item.name}</a>
                    </li>
                </#list>
            </ul>
        </nav>
    </header>
</#macro>

<#macro sidebar>
    <aside>
        <section>
            <div class="header">Information</div>
            <div class="body">Lorem ipsum dolor sit amet, consectetur adipisicing elit. Asperiores enim expedita facilis
                fugiat ipsum iste nobis reprehenderit tempore ut voluptatibus?
            </div>
            <div class="footer">
                <a href="#">View all</a>
            </div>
        </section>
        <section>
            <div class="header">Information</div>
            <div class="body">Lorem ipsum dolor sit amet, consectetur adipisicing elit. Asperiores enim expedita facilis
                fugiat ipsum iste nobis reprehenderit tempore ut voluptatibus?
            </div>
            <div class="footer">
                <a href="#">View all</a>
            </div>
        </section>
        <section>
            <div class="header">Information</div>
            <div class="body">Lorem ipsum dolor sit amet, consectetur adipisicing elit. Asperiores enim expedita facilis
                fugiat ipsum iste nobis reprehenderit tempore ut voluptatibus?
            </div>
            <div class="footer">
                <a href="#">View all</a>
            </div>
        </section>
    </aside>
</#macro>

<#macro footer>
    <footer>
        2010-2021 &copy; by Mike Mirzayanov
    </footer>
</#macro>

<#macro userlink user nameOnly=false>
    <#if nameOnly>
        <a href="/user?handle=${user.handle}">${user.name}</a>
    <#else>
        <a href="/user?handle=${user.handle}" class="colorful ${user.color?string?lower_case}">${user.handle}</a>
    </#if>
</#macro>

<#macro userlinksByArrows user>
<#-- @ftlvariable name="user" type="ru.itmo.web.hw4.model.User" -->
    <#assign prevu = findPrev(users, user, "handle")/>
    <#assign nextu = findNext(users, user, "handle")/>
    <div class="nav_arrows">
        <div class="arrows_container">
            <#if prevu?has_content>
                <a href="/user?handle=${prevu.handle}" class="prev_arrow">←</a>
            <#else>
                <span class="prev_arrow disabled">←</span>
            </#if>
            <#if nextu?has_content>
                <a href="/user?handle=${nextu.handle}" class="next_arrow">→</a>
            <#else>
                <span class="next_arrow disabled">→</span>
            </#if>
        </div>
    </div>
</#macro>

<#function findBy items key value>
    <#list items as item>
        <#if item[key] == value>
            <#return item/>
        </#if>
    </#list>
</#function>

<#function findNext items pivot key>
    <#list items as item>
        <#if item[key] == pivot[key]>
            <#return items[items?seq_index_of(item) + 1]!/>
        </#if>
    </#list>
</#function>

<#function findPrev items pivot key>
    <#list items as item>
        <#if item[key] == pivot[key]>
            <#return items[items?seq_index_of(item) - 1]!/>
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

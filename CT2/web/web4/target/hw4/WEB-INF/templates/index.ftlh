<#import "./common.ftlh" as c>

<@c.page>
    <article class = "posts">
        <#list posts?reverse as post>
            <@c.findPost post/>
        </#list>
    </article>
</@c.page>
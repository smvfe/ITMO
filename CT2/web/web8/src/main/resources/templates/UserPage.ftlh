<#import "commons.ftlh" as c>

<@c.page>
    <div class="user-profile">
        <#if userPage??>
            <h1>${userPage.login}</h1>
            <p><strong>Id:</strong> ${userPage.id}</p>
            <p><strong>Login:</strong> ${userPage.login}</p>
            <p><strong>Creation Time:</strong> ${userPage.creationTime}</p>
        <#else>
            <h1>${error}</h1>
        </#if>
    </div>
</@c.page>
<#-- @ftlvariable name="handle" type="java.lang.String" -->
<#-- @ftlvariable name="id" type="java.lang.Long" -->
<#import "./common.ftlh" as c>

<@c.page>
    <#if handle??>
        <#assign key = "handle">
        <#assign value = handle>
    <#elseif id??>
        <#assign key = "id">
        <#assign value = id>
    <#else>
        No such user
    </#if>
    <#assign u = c.findBy(users, key, value)!/>

    <#if u?has_content>
        <article class="profile_page">
            <div class="profile_container">
                <h1><@c.userlink u /></h1>
                <ul class="profile-info">
                    <li>id: ${u.id}</li>
                    <li>handle: ${u.handle}</li>
                    <li>name: ${u.name}</li>
                </ul>
            </div>
            <div class="profile_image">
                <img alt="User Picture" src = "/img/default_user_image.jpg">
            </div>
        </article>
        <@c.userlinksByArrows u/>
    <#else>
        No such user
    </#if>
</@c.page>

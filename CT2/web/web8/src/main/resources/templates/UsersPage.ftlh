<#import "commons.ftlh" as c>
<#import "/spring.ftl" as spring>
<@c.page>
    <div class="users datatable">
        <div class="caption">User</div>
        <table>
            <thead>
            <tr>
                <th>Id</th>
                <th>Login</th>
                <th>Created</th>
                <th>Disabled</th>
            </tr>
            </thead>
            <tbody>
            <#if users?size==0>
                <tr class="noData">
                    <td colspan="4">
                        No data
                    </td>
                </tr>
            <#else>
                <#list users as user>
                    <tr>
                        <td class="id">${user.id}</td>
                        <td class="login"><a href="<@spring.url '/user/' + user.id/>">${user.login}</a></td>
                        <td class="creationTime">${user.creationTime}</td>
                        <td class="actions">
                            <form method="post" action="/users/all">
                                <input type="hidden" name="id" value="${user.id}"/>
                                <#if user.disable>
                                    <input type="hidden" name="disable" value="false"/>
                                <#else>
                                    <input type="hidden" name="disable" value="true"/>
                                </#if>
                                <button type="submit">${user.disable?string("Enable", "Disable")}</button>
                            </form>
                        </td>
                    </tr>
                </#list>
            </#if>
            </tbody>
        </table>
    </div>
</@c.page>

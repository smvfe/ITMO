<#import "./common.ftlh" as c>

<@c.page>
    <article class ="user_table">
        <div>
            <table>
                <thead>
                <tr>
                    <th>Users</th>
                </tr>
                </thead>
                <tbody>
                <#list users as u>
                    <tr>
                        <td>
                                <@c.userlink u />
                        </td>
                    </tr>
                </#list>
                </tbody>
            </table>
        </div>
    </article>
</@c.page>
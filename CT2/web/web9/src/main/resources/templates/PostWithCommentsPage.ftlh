<#-- @ftlvariable name="comments" type="ru.itmo.wp.domain.Comment[]" -->
<#-- @ftlvariable name="comment" type="ru.itmo.wp.domain.Comment"-->
<#import "commons.ftlh" as c>

<@c.page>
    <@c.post post/>
    <#if user??>
        <#if post??>
        <div class="commentInput">
            <div class="form">
                <div class="header">Write Comment</div>
                <div class="body">
                    <form method="post" action="/post/${post.id}">
                        <div class="field">
                            <div class="name">
                                <label for="text">Text</label>
                            </div>
                            <div class="value">
                                <textarea id="text" name="text">${newComment.text!}</textarea>
                            </div>
                            <@c.error "newComment.text"/>
                        </div>
                        <div class="button-field">
                            <input type="submit" value="Write">
                        </div>
                    </form>
                </div>
            </div>
        </div>
        <#else>
            <div>No such post</div>
        </#if>
    </#if>
    <#list comments as comment>
        <@c.comment comment=comment/>
    </#list>
</@c.page>
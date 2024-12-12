<#-- @ftlvariable name="post" type="ru.itmo.wp.domain.Post" -->
<#-- @ftlvariable name="tagArray" type="java.lang.String" -->
<#import "/spring.ftl" as spring>
<#import "commons.ftlh" as c>

<@c.page>
    <div class="form">
        <div class="header">Write Post</div>
        <div class="body">
            <form method="post" action="">
                <input type="hidden" name="action" value="writePost"/>
                <div class="field">
                    <div class="name">
                        <label for="title">Title</label>
                    </div>
                    <div class="value">
                        <input autofocus id="title" name="title" value="${post.title!}"/>
                    </div>
                    <@c.error "post.title"/>
                </div>
                <div class="field">
                    <div class="name">
                        <label for="text">Text</label>
                    </div>
                    <div class="value">
                        <textarea id="text" name="text">${post.text!}</textarea>
                    </div>
                    <@c.error "post.text"/>
                </div>
                <div class="field">
                    <div class="name">
                        <label for="tagArray">Tags</label>
                    </div>
                    <div class="value">
                        <input type="text" id="tagArray" name="tagArray" value="${tagArray!}"/>
                    </div>
                </div>
                <div class="button-field">
                    <input type="submit" value="Write">
                </div>
            </form>
        </div>
    </div>
</@c.page>
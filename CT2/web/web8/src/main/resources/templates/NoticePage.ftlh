<#import "commons.ftlh" as c>

<@c.page>
    <form action="/notice" method="post">
        <label>
            <textarea name="content" placeholder="Enter notice content" required>${noticeForm.content!}</textarea>
        </label>
        <@c.error "noticeForm.content"/>
        <button type="submit">Add</button>
    </form>
</@c.page>
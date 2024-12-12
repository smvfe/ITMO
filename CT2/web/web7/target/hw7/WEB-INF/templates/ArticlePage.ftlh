<#import "commons.ftlh" as c>

<@c.page>
    <div class="create form-box">
        <div class="header">Create Article</div>
        <div class="body">
            <form method="post" action="">
                <div class="field">
                    <div class="name">
                        <label for="title">Title:</label>
                    </div>
                    <div class="value">
                        <input id="title" name="title" type="text"/>
                    </div>
                </div>
                <div class="field">
                    <div class="name">
                        <label for="text">Text:</label>
                    </div>
                    <div class="value">
                        <textarea id="text" name="text"></textarea>
                    </div>
                </div>
                <div class="error"></div>
                <div class="button-field">
                    <input type="submit" value="Create"/>
                </div>
            </form>
        </div>
    </div>
    <script>
        $(function () {
            $(".create form").submit(function () {
                const title = $(this).find("input[name='title']").val();
                const text = $(this).find("textarea[name='text']").val();
                const $error = $(this).find(".error");

                ajax('', {
                    action: 'create',
                    title: title,
                    text: text
                }, function(response) {
                    if (response.error) {
                        $error.text(response.error);
                    }
                });

                return false;
            });
        })
    </script>
</@c.page>

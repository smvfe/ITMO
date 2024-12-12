<#-- @ftlvariable name="login" type="java.lang.String" -->
<#-- @ftlvariable name="password" type="java.lang.String" -->
<#-- @ftlvariable name="error" type="java.lang.String" -->
<#import "commons.ftlh" as c>

<@c.page>
    <div class="enter form-box">
        <div class="header">Enter</div>
        <div class="body">
            <form method="post" action="">
                <input type="hidden" name="action" value="enter"/>
                <div class="field">
                    <div class="name">
                        <label for="loginOrEmail">Login/Email</label>
                    </div>
                    <div class="value">
                        <input id="loginOrEmail" name="loginOrEmail"/>
                    </div>
                </div>
                <div class="field">
                    <div class="name">
                        <label for="password">Password</label>
                    </div>
                    <div class="value">
                        <input id="password" name="password" type="password"/>
                    </div>
                </div>
                <div class="error"></div>
                <div class="button-field">
                    <input type="submit" value="Enter">
                </div>
            </form>
        </div>
    </div>
    <script>
        $(function () {
            $(".enter form").submit(function () {
                const loginOrEmail = $(this).find("input[name='loginOrEmail']").val();
                const password = $(this).find("input[name='password']").val();
                const $error = $(this).find(".error");

                ajax('', {
                    action: 'enter',
                    loginOrEmail: loginOrEmail,
                    password: password
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

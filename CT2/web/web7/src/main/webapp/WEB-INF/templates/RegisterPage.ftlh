<#import "commons.ftlh" as c>

<@c.page>
    <div class="registration form-box">
        <div class="header">Register</div>
        <div class="body">
            <form method="post" action="">
                <input type="hidden" name="action" value="register"/>
                <div class="field">
                    <div class="name">
                        <label for="login">Login</label>
                    </div>
                    <div class="value">
                        <input id="login" name="login" />
                    </div>
                </div>
                <div class="field">
                    <div class="name">
                        <label for="email">Email</label>
                    </div>
                    <div class="value">
                        <input id="email" name="email" />
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
                <div class="field">
                    <div class="name">
                        <label for="password">Confirm password</label>
                    </div>
                    <div class="value">
                        <input id="password" name="passwordConfirmation" type="password"/>
                    </div>
                </div>
                <div class="error"></div>
                <div class="button-field">
                    <input type="submit" value="Login">
                </div>
            </form>
        </div>
    </div>

    <script>
        $(function () {
            $(".registration form").submit(function () {
                const login = $(this).find("input[name='login']").val();
                const password = $(this).find("input[name='password']").val();
                const email = $(this).find("input[name='email']").val();
                const passwordConfirmation = $(this).find("input[name='passwordConfirmation']").val();
                const $error = $(this).find(".error");

                ajax('', {
                    action: 'register',
                    login: login,
                    email: email,
                    password: password,
                    passwordConfirmation: passwordConfirmation
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

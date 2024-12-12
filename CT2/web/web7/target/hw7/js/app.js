window.notify = function (message) {
    $.notify(message, {
        position: "right bottom",
        className: "success"
    });
}

function ajax(url, data, success) {
    $.ajax({
        type: 'POST',
        url: url,
        dataType: 'json',
        data: data,
        success: function(response) {
            if (response.redirect) {
                location.href = response.redirect;
            } else {
                success(response);
            }
        }
    });
}
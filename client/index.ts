/// <reference path="jquery.d.ts" />

$(function() {

  $('#R-rand-vec').submit(function (e: JQueryEventObject) {
    $.get('R-rand-vec', function (res: string) {
      $('#R-rand-vec-result').text(res);
    }).fail(function (jqxhr, status, err) { console.log(status);console.log(err); });
    e.preventDefault();
    return false;
  });

  $('#R-fib').submit(function (e: JQueryEventObject) {
    $.get('R-fib', $(this).serialize(), function (res: string) {
      $('#R-fib-param-result').text(res);
    });
    e.preventDefault();
    return false;
  });

  $('#R-data-frame').submit(function (e: JQueryEventObject) {
    $.get('R-df', function (res: string) {
      $('#R-df-result').text(res);
    });
    e.preventDefault();
    return false;
  });
});

/// <reference path="jquery.d.ts" />
$(function() {

  $('#R-rand-vec').submit(function (e) {
    $.get('R-rand-vec', function (res) {
      $('#R-rand-vec-result').text(res);
    });
    e.preventDefault();
    return false;
  });

  $('#R-fib').submit(function (e) {
    $.get('R-fib', $(this).serialize(), function (res) {
      $('#R-fib-param-result').text(res);
    });
    e.preventDefault();
    return false;
  });
});

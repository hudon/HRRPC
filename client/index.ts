/// <reference path="jquery.d.ts" />

interface RPCResult {
  HRRPC: string;
  result: any;
}

function parseRPCResult (res: string): RPCResult {
  return JSON.parse(res);
}

$(function() {

  $('#R-rand-vec').submit(function (e) {
    $.get('R-rand-vec', function (res: string) {
      $('#R-rand-vec-result').text(parseRPCResult(res).result);
    });
    e.preventDefault();
    return false;
  });

  $('#R-fib').submit(function (e) {
    $.get('R-fib', $(this).serialize(), function (res: string) {
      $('#R-fib-param-result').text(parseRPCResult(res).result);
    });
    e.preventDefault();
    return false;
  });
});

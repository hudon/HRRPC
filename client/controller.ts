/// <reference path="angular.d.ts" />

module TStat {
  export interface IRPCScope extends ng.IScope {
    randVec: number[];
    fibRes: number;
    sampleDF: {};
    getFib: (number) => void;
    getRandVec: () => void;
    getSampleDF: () => void;
  }

  interface IRPCRes {
    HRRPC: string;
    result: any;
  }

  function toRPCConfig (method: string, params: any): ng.IRequestConfig {
    return { params: { rpc: { method: method, params: params} } };
  }

  export function RPCController($scope: IRPCScope, $http: ng.IHttpService): void {
    function doRPC (method: string, params: any, callback: any) {
      $http.get('/R2', toRPCConfig(method, params))
        .success(callback)
        .error((data, status, headers, config) => {
            // TODO proper error handling
            alert(data);
        });
    }

    $scope.getRandVec = () => {
      doRPC('user_func', [], (data: IRPCRes) => {
          $scope.randVec = data.result;
      });
    };

    $scope.getFib = (fibParam: number) => {
      doRPC('fib', fibParam, (data: IRPCRes) => {
        $scope.fibRes = data.result;
      });
    }

    $scope.getSampleDF = () => {
      doRPC('SomeDF', [], (data: IRPCRes) => {
        $scope.sampleDF = data.result;
      });
    }
  }
}

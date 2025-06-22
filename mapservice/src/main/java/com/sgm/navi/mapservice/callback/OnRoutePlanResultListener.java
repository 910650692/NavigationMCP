package com.sgm.navi.mapservice.callback;

public interface OnRoutePlanResultListener {

    /**
     * 算路失败回调.
     *
     * @param errorCode int,错误码.
     * @param errorMsg String,错误信息.
     */
    void onRoutePlanError(int errorCode, String errorMsg);

    /**
     * 路线规划成功回调.
     *
     * @param routeResult String, BaseRouteResult对应的json类型的字符串.
     */
    void onRoutePlanResult(String routeResult);
}

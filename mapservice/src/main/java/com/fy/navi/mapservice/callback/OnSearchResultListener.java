package com.fy.navi.mapservice.callback;

import android.util.Log;

import com.fy.navi.mapservice.bean.INaviConstant;

public interface OnSearchResultListener {

    /**
     * 搜索失败回调.
     *
     * @param silent boolean, 是否静默搜索.
     * @param errorCode int,错误码.
     */
    default void onSearchError(boolean silent, int errorCode) {
        Log.d(INaviConstant.NAVI_COMMON_TAG, "searchError: " + silent + ", code: " + errorCode);
    }

    /**
     * 搜索结果回调.
     *
     * @param silent boolean, 是否静默搜索.
     * @param result String, BaseSearchResult对应的json类型字符串.
     */
    default void onSearchResult(boolean silent, String result) {
        Log.d(INaviConstant.NAVI_COMMON_TAG, "searchSuccess, silent:" + silent + ", result: " + result);
    }

    /**
     * 逆地理搜索结果回调.
     *
     * @param taskId int, 搜索task唯一标识.
     * @param reverseResult String, BaseSearchPoi对应的json类型字符串.
     */
    default void onReverseGeoSearchResult(int taskId, String reverseResult) {
        Log.d(INaviConstant.NAVI_COMMON_TAG, "reverseSearch, taskId:" + taskId + ", result: " + reverseResult);
    }
}

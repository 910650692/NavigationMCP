// INaviAutoSearchCallback.aidl
package com.fy.navi.mapservice.common;

interface INaviAutoSearchCallback {
    void onSearchFailed(boolean silent, int errorCode);

    void onSearchResult(boolean silent, String searchResult);

    void onReverseGeoSearchResult(int taskId, String reverseResult);
}
package com.fy.navi.mapservice.callback;

public interface OnSearchResultListener {

    default void onSearchError(boolean silent, int errorCode) {}

    default void onSearchResult(boolean silent, String result) {}

    default void onReverseGeoSearchResult(int taskId, String reverseResult) {};
}

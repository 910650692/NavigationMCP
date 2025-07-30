package com.sgm.navi.service.logicpaket.search;

import com.sgm.navi.service.adapter.search.cloudByPatac.rep.BaseRep;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.search.SearchResultEntity;


/**
 * 搜索结果回调接口
 * 定义了两种搜索结果的回调方法：正常搜索结果回调和静默搜索结果回调
 */
public interface SearchResultCallback {

    /**
     * 正常搜索结果回调方法
     * 在完成搜索操作后被调用，用于返回搜索结果给 HMI处理界面
     *
     * @param taskId             taskId,请求的唯一标识
     * @param errorCode          错误码，表示搜索操作的结果状态
     * @param message            错误消息，描述搜索操作的结果信息
     * @param searchResultEntity 搜索结果 {@link SearchResultEntity}，包含具体的搜索结果数据
     */
    default void onSearchResult(int taskId, int errorCode, String message, SearchResultEntity searchResultEntity) {

    }

    /**
     * 静默搜索结果回调方法
     * 在完成静默搜索操作后被调用，用于返回搜索结果
     *
     * @param taskId             taskId,请求的唯一标识
     * @param errorCode          错误码，表示搜索操作的结果状态
     * @param message            错误消息，描述搜索操作的结果信息
     * @param searchResultEntity 搜索结果 {@link SearchResultEntity}，包含具体的搜索结果数据
     */
    default void onSilentSearchResult(int taskId, int errorCode, String message, SearchResultEntity searchResultEntity) {

    }

    /**
     * 搜索扎标点击事件
     * @param poiInfoEntity 点击的下标对象
     */
    default void onMarkClickCallBack(final PoiInfoEntity poiInfoEntity) {

    }

    /**
     * 搜索子点扎标点击事件
     * @param index 下标点击事件
     */
    default void onMarkChildClickCallBack(final int index) {

    }

    /**
     * 搜索子点/终点停车场扎标点击事件
     * @param index 下标点击事件
     */
    default void onMarkTerminalParkClickCallBack(final int index) {

    }


    /**
     * 语音触发筛选，通过此接口传递到搜索结果界面.
     * @param mapTypeId MapTypeId.
     * @param sortValue String，筛选规则.
     */
    default void onVoicePoiSort(MapType mapTypeId, String sortValue) {

    }

    /**
     * 语音触发筛选，通过此接口传递到搜索结果界面.
     * @param mapTypeId MapTypeId.
     * @param sortValue String，筛选规则.
     */
    default void onVoicePoiSort(MapType mapTypeId, String sortValue, GeoPoint point) {

    }

    /**
     * 成功的搜索结果回调
     * @param taskId 任务ID
     * @param searchKey 任务来源
     * @param result 搜索结果
     */
    default void onNetSearchResult(int taskId,String searchKey,BaseRep result) {
    }

    /**
     * 网络接口失败回调
     *
     * @param taskId 任务ID
     * @param searchKey 任务来源
     * @param message 错误消息
     */
    default void onNetSearchResultError(int taskId,String searchKey,String message){

    }

    /**
     * 搜索结果列表页面可变状态变化回调
     * @param isShow 是否可见
     */
    default void onShowStateChanged(boolean isShow) {

    }

    default void onTipDialog(String status){

    }
}


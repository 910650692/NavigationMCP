package com.fy.navi.service.logicpaket.search;

import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;


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
    void onSearchResult(int taskId, int errorCode, String message, SearchResultEntity searchResultEntity);

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
     * @param poiInfo
     */
    default void onMarkClickCallBack(PoiInfoEntity poiInfo) {

    }
}


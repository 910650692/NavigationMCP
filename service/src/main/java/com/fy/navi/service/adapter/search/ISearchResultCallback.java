package com.fy.navi.service.adapter.search;

import com.fy.navi.service.adapter.search.cloudByPatac.rep.BaseRep;
import com.fy.navi.service.define.search.SearchErrorCode;
import com.fy.navi.service.define.search.SearchRequestParameter;
import com.fy.navi.service.define.search.SearchResultEntity;

/**
 * @version \$Revision1.0\$
 * @author baipeng0904
 * 搜索结果回调接口
 * 定义了两种搜索结果的回调方法：正常搜索结果回调和静默搜索结果回调
 */
public interface ISearchResultCallback {
    /**
     * 正常搜索结果回调方法
     * 在完成搜索操作后被调用，用于返回搜索结果给 HMI处理界面
     *
     * @param errorCode          错误码，表示搜索操作的结果状态{@link SearchErrorCode.ErrorCode}
     * @param message            错误消息，描述搜索操作的结果信息
     * @param searchResultEntity 搜索结果 {@link SearchResultEntity}，包含具体的搜索结果数据
     * @param taskId             任务ID
     * @param requestParameterBuilder 搜索请求参数 {@link SearchRequestParameter}
     */
    void onSearchResult(int taskId, @SearchErrorCode.ErrorCode int errorCode, String message,
                        SearchResultEntity searchResultEntity, final SearchRequestParameter requestParameterBuilder);

    /**
     * 静默搜索结果回调方法
     * 在完成静默搜索操作后被调用，用于返回搜索结果
     *
     * @param errorCode          错误码，表示搜索操作的结果状态{@link SearchErrorCode.ErrorCode}
     * @param message            错误消息，描述搜索操作的结果信息
     * @param searchResultEntity 搜索结果 {@link SearchResultEntity}，包含具体的搜索结果数据
     * @param taskId             任务ID
     */
    void onSilentSearchResult(int taskId, @SearchErrorCode.ErrorCode int errorCode, String message, SearchResultEntity searchResultEntity);
    // 网络接口回调
    void onNetSearchResult(int taskId,String searchKey,BaseRep result);
}

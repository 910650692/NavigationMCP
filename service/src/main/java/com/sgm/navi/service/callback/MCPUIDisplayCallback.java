package com.sgm.navi.service.callback;

import com.sgm.navi.service.define.search.SearchRequestParameter;
import com.sgm.navi.service.define.search.SearchResultEntity;

/**
 * MCP UI显示回调接口
 * 用于通知应用层显示搜索结果界面
 */
public interface MCPUIDisplayCallback {
    
    /**
     * 显示搜索结果界面
     * 
     * @param taskId 搜索任务ID
     * @param searchResultEntity 搜索结果实体
     * @param requestParameter 搜索请求参数
     */
    void onShowSearchResult(int taskId, SearchResultEntity searchResultEntity, SearchRequestParameter requestParameter);
}
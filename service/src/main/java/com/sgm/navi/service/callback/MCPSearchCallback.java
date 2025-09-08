package com.sgm.navi.service.callback;

import com.sgm.navi.service.define.search.SearchResultEntity;

/**
 * MCP搜索回调接口
 * 用于解耦service模块和app模块的依赖关系
 * 支持逆地理搜索、关键词搜索、周边搜索等所有MCP搜索功能
 */
public interface MCPSearchCallback {
    /**
     * 完成geoSearch任务
     * 
     * @param taskId 任务ID
     * @param address 解析得到的地址，可以为null
     */
    void completeGeoSearchTask(int taskId, String address);
    
    /**
     * 从搜索结果中提取地址信息
     * 
     * @param searchResultEntity 搜索结果
     * @return 地址字符串，提取失败返回null
     */
    String extractAddressFromSearchResult(SearchResultEntity searchResultEntity);
    
    /**
     * 完成关键词搜索任务
     * 
     * @param taskId 任务ID
     * @param searchResult 搜索结果的JSON字符串，可以为null
     */
    void completeKeywordSearchTask(int taskId, String searchResult);
    
    /**
     * 从搜索结果中提取POI列表并格式化为JSON
     * 
     * @param searchResultEntity 搜索结果
     * @param keyword 搜索关键词
     * @param maxResults 最大结果数
     * @return JSON格式的搜索结果，提取失败返回null
     */
    String extractPOIListFromSearchResult(SearchResultEntity searchResultEntity, String keyword, int maxResults);
    
    /**
     * 完成周边搜索任务
     * 
     * @param taskId 任务ID
     * @param searchResult 搜索结果的JSON字符串，可以为null
     */
    void completeAroundSearchTask(int taskId, String searchResult);
}
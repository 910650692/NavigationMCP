package com.sgm.navi.mcp.tools;

import android.util.Log;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonObject;
import com.sgm.navi.service.callback.MCPSearchCallback;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.search.SearchResultEntity;
import com.sgm.navi.service.adapter.position.PositionAdapter;
import com.sgm.navi.service.define.position.LocInfoBean;
import com.sgm.navi.service.logicpaket.search.SearchPackage;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.List;
import java.util.ArrayList;

/**
 * MCP工具基础类
 * 提供所有MCP工具共享的通用功能和方法
 */
public class BaseToolHelper implements MCPSearchCallback {
    private static final String TAG = "BaseToolHelper";
    
    // 单例实例
    private static volatile BaseToolHelper instance;
    
    // 共享的Gson实例
    protected final Gson gson = new GsonBuilder()
        .disableHtmlEscaping()
        .create();
    
    // 静态任务管理器：存储待完成的geoSearch CompletableFuture
    private static final ConcurrentHashMap<Integer, CompletableFuture<String>> pendingGeoSearchTasks = new ConcurrentHashMap<>();
    
    // 静态任务管理器：存储待完成的keywordSearch CompletableFuture
    private static final ConcurrentHashMap<Integer, CompletableFuture<String>> pendingKeywordSearchTasks = new ConcurrentHashMap<>();
    
    // 静态任务管理器：存储待完成的aroundSearch CompletableFuture
    private static final ConcurrentHashMap<Integer, CompletableFuture<String>> pendingAroundSearchTasks = new ConcurrentHashMap<>();
    
    /**
     * 私有构造函数，单例模式
     */
    private BaseToolHelper() {
    }
    
    /**
     * 获取单例实例
     */
    public static BaseToolHelper getInstance() {
        if (instance == null) {
            synchronized (BaseToolHelper.class) {
                if (instance == null) {
                    instance = new BaseToolHelper();
                }
            }
        }
        return instance;
    }
    
    /**
     * 创建错误响应JSON
     */
    public JsonObject createErrorResponse(String error, String message) {
        JsonObject errorJson = new JsonObject();
        errorJson.addProperty("status", "error");
        errorJson.addProperty("error", error);
        if (message != null) {
            errorJson.addProperty("message", message);
        }
        errorJson.addProperty("timestamp", System.currentTimeMillis());
        return errorJson;
    }
    
    /**
     * 创建成功响应JSON
     */
    public JsonObject createSuccessResponse() {
        JsonObject successJson = new JsonObject();
        successJson.addProperty("status", "success");
        successJson.addProperty("timestamp", System.currentTimeMillis());
        return successJson;
    }
    
    /**
     * 添加geoSearch任务
     */
    public void addGeoSearchTask(int taskId, CompletableFuture<String> future) {
        pendingGeoSearchTasks.put(taskId, future);
        // Log.d(TAG, "添加geoSearch任务: " + taskId);
    }
    
    /**
     * 添加keywordSearch任务
     */
    public void addKeywordSearchTask(int taskId, CompletableFuture<String> future) {
        pendingKeywordSearchTasks.put(taskId, future);
        Log.d(TAG, "添加keywordSearch任务: " + taskId);
    }
    
    /**
     * 添加aroundSearch任务
     */
    public void addAroundSearchTask(int taskId, CompletableFuture<String> future) {
        pendingAroundSearchTasks.put(taskId, future);
        Log.d(TAG, "添加aroundSearch任务: " + taskId);
    }
    
    /**
     * 移除geoSearch任务
     */
    public CompletableFuture<String> removeGeoSearchTask(int taskId) {
        return pendingGeoSearchTasks.remove(taskId);
    }
    
    /**
     * 移除keywordSearch任务
     */
    public CompletableFuture<String> removeKeywordSearchTask(int taskId) {
        return pendingKeywordSearchTasks.remove(taskId);
    }
    
    /**
     * 移除aroundSearch任务
     */
    public CompletableFuture<String> removeAroundSearchTask(int taskId) {
        return pendingAroundSearchTasks.remove(taskId);
    }
    
    /**
     * 检查指定taskId是否为MCP任务
     * @param taskId 任务ID
     * @return true表示是MCP任务，false表示不是MCP任务
     */
    public static boolean isMCPTask(int taskId) {
        return pendingGeoSearchTasks.containsKey(taskId) || 
               pendingKeywordSearchTasks.containsKey(taskId) || 
               pendingAroundSearchTasks.containsKey(taskId);
    }
    
    /**
     * 修复字符串编码问题
     * 尝试检测和修复可能的UTF-8编码问题
     */
    public String fixEncoding(String input) {
        if (input == null || input.isEmpty()) {
            return input;
        }
        
        try {
            // 检查是否包含乱码字符
            if (input.contains("�")) {
                // 尝试使用ISO-8859-1重新编码为UTF-8
                byte[] bytes = input.getBytes("ISO-8859-1");
                String fixed = new String(bytes, "UTF-8");
                Log.d(TAG, "尝试编码修复: " + input + " -> " + fixed);
                return fixed;
            }
        } catch (Exception e) {
            Log.w(TAG, "编码修复失败: " + e.getMessage());
        }
        
        return input;
    }
    
    /**
     * 获取当前位置信息
     */
    public LocInfoBean getCurrentPosition() {
        try {
            return PositionAdapter.getInstance().getLastCarLocation();
        } catch (Exception e) {
            Log.e(TAG, "获取当前位置失败", e);
            return null;
        }
    }
    
    /**
     * 清理超时的任务 - 防止内存泄漏
     */
    public static void cleanupTimeoutTasks() {
        if (!pendingGeoSearchTasks.isEmpty()) {
            Log.d(TAG, "清理geoSearch任务，当前待处理任务数: " + pendingGeoSearchTasks.size());
            // 移除已完成的任务
            pendingGeoSearchTasks.entrySet().removeIf(entry -> entry.getValue().isDone());
        }
        
        if (!pendingKeywordSearchTasks.isEmpty()) {
            Log.d(TAG, "清理keywordSearch任务，当前待处理任务数: " + pendingKeywordSearchTasks.size());
            // 移除已完成的任务
            pendingKeywordSearchTasks.entrySet().removeIf(entry -> entry.getValue().isDone());
        }
    }
    
    // ==================== MCPSearchCallback 接口实现 ====================
    
    /**
     * 完成geoSearch任务 - 实现MCPSearchCallback接口
     * 
     * @param taskId 任务ID
     * @param address 解析得到的地址，可以为null
     */
    @Override
    public void completeGeoSearchTask(int taskId, String address) {
        CompletableFuture<String> future = pendingGeoSearchTasks.remove(taskId);
        if (future != null && !future.isDone()) {
            future.complete(address);
            // Log.d(TAG, "geoSearch任务完成: taskId=" + taskId + ", address=" + address);
        }
    }
    
    /**
     * 从搜索结果中提取地址信息 - 实现MCPSearchCallback接口
     * 
     * @param searchResultEntity 搜索结果
     * @return 地址字符串，提取失败返回null
     */
    @Override
    public String extractAddressFromSearchResult(SearchResultEntity searchResultEntity) {
        try {
            if (searchResultEntity != null && searchResultEntity.getPoiList() != null 
                && !searchResultEntity.getPoiList().isEmpty()) {
                
                PoiInfoEntity firstPoi = searchResultEntity.getPoiList().get(0);
                String address = firstPoi.getAddress();
                
                // 如果地址为空，尝试使用POI名称
                if (address == null || address.trim().isEmpty()) {
                    address = firstPoi.getName();
                }
                
                // Log.d(TAG, "提取地址信息: " + address);
                return address;
            }
        } catch (Exception e) {
            Log.w(TAG, "提取地址信息失败: " + e.getMessage());
        }
        return null;
    }
    
    /**
     * 完成关键词搜索任务 - 实现MCPSearchCallback接口
     * 
     * @param taskId 任务ID
     * @param searchResult 搜索结果的JSON字符串，可以为null
     */
    @Override
    public void completeKeywordSearchTask(int taskId, String searchResult) {
        CompletableFuture<String> future = pendingKeywordSearchTasks.remove(taskId);
        if (future != null && !future.isDone()) {
            future.complete(searchResult);
            Log.d(TAG, "关键词搜索任务完成: taskId=" + taskId + ", result=" + (searchResult != null ? "成功" : "无结果"));
        }
    }
    
    /**
     * 从搜索结果中提取POI列表并格式化为JSON - 实现MCPSearchCallback接口
     * 
     * @param searchResultEntity 搜索结果
     * @param keyword 搜索关键词
     * @param maxResults 最大结果数
     * @return JSON格式的搜索结果，提取失败返回null
     */
    @Override
    public String extractPOIListFromSearchResult(SearchResultEntity searchResultEntity, String keyword, int maxResults) {
        try {
            JsonObject result = new JsonObject();
            result.addProperty("keyword", keyword);
            
            if (searchResultEntity != null && searchResultEntity.getPoiList() != null 
                && !searchResultEntity.getPoiList().isEmpty()) {
                
                List<PoiInfoEntity> poiList = searchResultEntity.getPoiList();
                List<JsonObject> resultList = new ArrayList<>();
                
                // 获取当前位置用于计算距离
                LocInfoBean currentLocation = getCurrentPosition();
                
                // 处理POI列表，限制数量
                int count = Math.min(poiList.size(), maxResults);
                for (int i = 0; i < count; i++) {
                    PoiInfoEntity poi = poiList.get(i);
                    JsonObject poiJson = new JsonObject();
                    
                    poiJson.addProperty("poi_id", poi.getPid() != null ? poi.getPid() : "poi_" + i);
                    poiJson.addProperty("name", poi.getName() != null ? poi.getName() : "未知地点");
                    poiJson.addProperty("address", poi.getAddress() != null ? poi.getAddress() : "");
                    poiJson.addProperty("latitude", poi.getPoint().getLat());
                    poiJson.addProperty("longitude", poi.getPoint().getLon());
                    
                    // 计算距离 - 使用现有的SearchPackage.calcStraightDistance方法
                    String distance = "未知距离";
                    if (currentLocation != null && poi.getPoint() != null) {
                        try {
                            distance = SearchPackage.getInstance().calcStraightDistance(poi.getPoint());
                            if (distance == null || distance.trim().isEmpty()) {
                                distance = "未知距离";
                            }
                        } catch (Exception e) {
                            Log.w(TAG, "计算距离失败: " + e.getMessage());
                        }
                    }
                    poiJson.addProperty("distance", distance);
                    
                    // POI类型信息
                    poiJson.addProperty("type", poi.getTypeCode() != null ? poi.getTypeCode() : "地点");
                    
                    // 电话信息
                    if (poi.getPhone() != null && !poi.getPhone().trim().isEmpty()) {
                        poiJson.addProperty("phone", poi.getPhone());
                    }
                    
                    // 排序相关字段 - Agent可用于自主排序
                    // 评分信息
                    if (poi.getRating() != null && !poi.getRating().trim().isEmpty()) {
                        poiJson.addProperty("rating", poi.getRating());
                    }
                    
                    // 价格信息
                    poiJson.addProperty("average_cost", poi.getAverageCost());
                    
                    resultList.add(poiJson);
                }
                
                result.addProperty("total_results", resultList.size());
                result.add("results", gson.toJsonTree(resultList));
                
                Log.d(TAG, "成功提取POI列表: " + resultList.size() + " 个结果");
                
            } else {
                result.addProperty("total_results", 0);
                result.add("results", gson.toJsonTree(new ArrayList<>()));
                result.addProperty("message", "搜索无结果");
            }
            
            return gson.toJson(result);
            
        } catch (Exception e) {
            Log.e(TAG, "提取POI列表失败: " + e.getMessage());
            return null;
        }
    }
    
    /**
     * 完成周边搜索任务 - 实现MCPSearchCallback接口
     * 
     * @param taskId 任务ID
     * @param searchResult 搜索结果的JSON字符串，可以为null
     */
    @Override
    public void completeAroundSearchTask(int taskId, String searchResult) {
        CompletableFuture<String> future = pendingAroundSearchTasks.remove(taskId);
        if (future != null && !future.isDone()) {
            future.complete(searchResult);
            Log.d(TAG, "周边搜索任务完成: taskId=" + taskId + ", result=" + (searchResult != null ? "成功" : "无结果"));
        }
    }
}
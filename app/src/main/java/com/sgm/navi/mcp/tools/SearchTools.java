package com.sgm.navi.mcp.tools;

import android.util.Log;
import com.google.gson.JsonObject;
import com.google.gson.JsonArray;
// 新MCP SDK注解
import com.sgm.mcp.protocol.annotations.McpTool;
import com.sgm.mcp.protocol.annotations.McpParam;
import com.sgm.mcp.protocol.McpSpec;
import com.sgm.mcp.protocol.Prompt;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.position.LocInfoBean;
import com.sgm.navi.service.logicpaket.search.SearchPackage;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.ArrayList;

/**
 * 搜索相关MCP工具类
 * 负责处理POI搜索、附近搜索等搜索相关功能
 */
public class SearchTools {
    private static final String TAG = "SearchTools";
    private final BaseToolHelper baseHelper = BaseToolHelper.getInstance();

    /**
     * 搜索地点(POI)
     */
    @McpTool(
        name = "search_poi",
        title = "地点搜索工具",
        description = "搜索任意地点(POI)，不限于用户当前位置附近。适用场景：1)搜索特定地点名称(如'上海迪士尼') 2)跨城市搜索 3)不需要基于当前位置的通用搜索。注意：此工具仅返回搜索结果数据，不会显示UI界面。",
        returnDescription = "返回包含POI信息的JSON数组，包括名称、地址、坐标等详细信息",
        promptMessageContent = McpSpec.PromptMessageContent.Text,
        readOnlyHint = true
    )
    public String searchPoi(
            @McpParam(name = "keyword", 
                      description = "搜索关键词。格式：字符串类型。示例：\"星巴克\"、\"上海迪士尼\"、\"加油站\"。注意：必须是字符串，不要传入对象或数组",
                      required = true)
            String keyword,
            @McpParam(name = "max_results", 
                      description = "最大返回结果数。格式：整数类型。示例：5、10。范围：1-10，默认5。注意：必须是数字，不要传入字符串",
                      required = false,
                      defaultValue = "5")
            Integer maxResults) {
        
        Log.d(TAG, "搜索POI: " + keyword);
        
        try {
            // 处理最大结果数参数
            int maxResultsNum = 5; // 默认值
            if (maxResults != null) {
                // 限制在1-10之间
                maxResultsNum = Math.max(1, Math.min(10, maxResults));
            }
            
            Log.d(TAG, "开始关键词搜索，关键词: " + keyword + ", 最大结果数: " + maxResultsNum);
            
            // 创建CompletableFuture
            CompletableFuture<String> future = new CompletableFuture<>();
            
            // 设置超时，超时返回null
            future = future.completeOnTimeout(null, 6000, TimeUnit.MILLISECONDS);
            
            // 发起关键词搜索，使用MCP专用方法确保回调被触发
            int taskId = SearchPackage.getInstance().keywordSearchForMCP(1, keyword, true);
            Log.d(TAG, "关键词搜索taskId: " + taskId);
            
            if (taskId <= 0) {
                Log.e(TAG, "关键词搜索taskId无效: " + taskId);
                JsonObject error = baseHelper.createErrorResponse("搜索启动失败", "搜索引擎返回无效的任务ID");
                error.addProperty("keyword", keyword);
                return Prompt.text(baseHelper.gson.toJson(error));
            }
            
            // 将taskId和future关联
            baseHelper.addKeywordSearchTask(taskId, future);
            
            // 等待结果，最多6秒
            String result = future.get(6000, TimeUnit.MILLISECONDS);
            
            // 清理任务
            baseHelper.removeKeywordSearchTask(taskId);
            
            if (result != null) {
                Log.d(TAG, "关键词搜索成功: " + result);
                return Prompt.text(result);
            } else {
                // 搜索超时或无结果
                JsonObject noResult = new JsonObject();
                noResult.addProperty("keyword", keyword);
                noResult.addProperty("total_results", 0);
                noResult.add("results", baseHelper.gson.toJsonTree(new ArrayList<>()));
                noResult.addProperty("message", "搜索超时或无结果");
                
                String noResultJson = baseHelper.gson.toJson(noResult);
                Log.w(TAG, "关键词搜索无结果: " + noResultJson);
                return noResultJson;
            }
            
        } catch (TimeoutException e) {
            Log.w(TAG, "关键词搜索超时");
            JsonObject error = baseHelper.createErrorResponse("搜索超时，请重试", null);
            error.addProperty("keyword", keyword);
            return Prompt.text(baseHelper.gson.toJson(error));
        } catch (Exception e) {
            Log.e(TAG, "关键词搜索失败", e);
            JsonObject error = baseHelper.createErrorResponse("搜索失败", e.getMessage());
            error.addProperty("keyword", keyword);
            return Prompt.text(baseHelper.gson.toJson(error));
        }
    }

    /**
     * 搜索附近的兴趣点 - 使用真实的周边搜索功能
     */
    @McpTool(
        name = "search_nearby_poi",
        title = "周边POI搜索工具",
        description = "搜索用户当前位置附近的POI。使用指南：【何时设置show_ui=true】仅当用户明确要求'显示'、'展示'、'打开地图'、'在地图上标记'时。【何时设置show_ui=false(默认)】用户询问'附近有什么'、'查询周边'、'搜索附近'等仅需要信息时。注意：大多数情况下应该使用false，只返回数据供分析。支持按距离、评分、价格排序。",
        returnDescription = "返回周边POI列表，包含名称、距离、评分、地址等信息，可选择是否显示UI界面",
        promptMessageContent = McpSpec.PromptMessageContent.Text,
        readOnlyHint = true,
        openWorldHint = true
    )
    public String searchNearbyPoi(
            @McpParam(name = "radius", 
                      description = "搜索半径(米)。格式：整数类型。示例：1000、2000。范围：500-10000，默认2000。注意：必须是数字，不要传入字符串",
                      required = false,
                      defaultValue = "2000")
            Integer searchRadius,
            @McpParam(name = "sort_type", 
                      description = "排序方式。格式：字符串类型。示例：\"DISTANCE_ASC\"、\"RATING_DESC\"、\"PRICE_LOW\"、\"PRICE_HIGH\"。默认DISTANCE_ASC。注意：必须使用引号包围",
                      required = false,
                      defaultValue = "DISTANCE_ASC")
            String sortType,
            @McpParam(name = "page_size", 
                      description = "每页大小。格式：整数类型。示例：10、15、20。范围：1-20，默认10。注意：必须是数字，不要传入字符串",
                      required = false,
                      defaultValue = "10")
            Integer pageSize,
            @McpParam(name = "page_num", 
                      description = "页码。格式：整数类型。示例：1、2、3。从1开始，默认1。注意：必须是数字，不要传入字符串",
                      required = false,
                      defaultValue = "1")
            Integer pageNum,
            @McpParam(name = "keyword", 
                      description = "搜索关键词。格式：字符串类型。示例：\"加油站\"、\"餐厅\"、\"停车场\"。注意：必须是字符串，不要传入对象或数组",
                      required = true)
            String keyword,
            @McpParam(name = "show_ui", 
                      description = "是否显示UI界面。【重要】默认false，仅返回数据。只有当用户明确使用'显示'、'展示'、'打开'等动词时才设为true。示例：'附近有哪些餐厅'=false(查询)，'显示附近餐厅'=true(展示)。格式：布尔值，不要传入字符串",
                      required = false,
                      defaultValue = "false")
            Boolean showUi) {
        
        // 搜索前清理之前的搜索标记，避免图层叠加
        try {
            SearchPackage.getInstance().clearLabelMark();
            Log.d(TAG, "已清理之前的搜索标记");
        } catch (Exception e) {
            Log.w(TAG, "清理搜索标记失败: " + e.getMessage());
        }
        
        // 检查参数格式 - 如果缺少必要的keyword参数，很可能是使用了错误的参数格式
        if (keyword == null || keyword.trim().isEmpty()) {
            Log.e(TAG, "缺少必要的keyword参数，可能使用了错误的参数格式");
            Log.e(TAG, "接收到的参数 - radius: " + searchRadius + ", sort_type: " + sortType + 
                      ", page_size: " + pageSize + ", page_num: " + pageNum + ", keyword: " + keyword);
            
            JsonObject error = new JsonObject();
            error.addProperty("status", "error");
            error.addProperty("error", "参数格式错误");
            error.addProperty("message", "search_nearby_poi需要keyword参数，请检查参数格式");
            
            // 添加正确的参数格式示例
            JsonObject expectedFormat = new JsonObject();
            expectedFormat.addProperty("keyword", "餐厅");
            expectedFormat.addProperty("radius", 1000);
            expectedFormat.addProperty("sort_type", "DISTANCE_ASC");
            expectedFormat.addProperty("page_size", 10);
            expectedFormat.addProperty("page_num", 1);
            
            error.add("expected_format", expectedFormat);
            error.addProperty("hint", "请使用keyword参数指定搜索内容，例如: {\"keyword\": \"餐厅\", \"radius\": 1000}");
            error.addProperty("note", "如果需要使用经纬度坐标搜索，请考虑使用其他工具或联系开发者");
            
            return Prompt.text(baseHelper.gson.toJson(error));
        }
        
        Log.d(TAG, "搜索附近POI: " + keyword + ", 半径: " + searchRadius + "米, 排序: " + sortType);
        
        try {
            // 参数验证和默认值处理
            if (searchRadius == null) searchRadius = 2000;
            if (pageSize == null) pageSize = 10;
            if (pageNum == null) pageNum = 1;
            if (sortType == null) sortType = "DISTANCE_ASC";
            if (showUi == null) showUi = false;
            
            // 参数范围限制
            searchRadius = Math.max(500, Math.min(10000, searchRadius));
            pageSize = Math.max(1, Math.min(20, pageSize));
            pageNum = Math.max(1, pageNum);
            
            // 获取当前位置作为搜索中心点
            LocInfoBean currentLocation = baseHelper.getCurrentPosition();
            if (currentLocation == null) {
                JsonObject error = baseHelper.createErrorResponse("获取当前位置失败", "无法确定搜索中心点");
                return Prompt.text(baseHelper.gson.toJson(error));
            }
            
            // 创建搜索中心点
            GeoPoint centerPoint = new GeoPoint();
            centerPoint.setLat(currentLocation.getLatitude());
            centerPoint.setLon(currentLocation.getLongitude());
            
            Log.d(TAG, "使用真实周边搜索功能，中心点: " + centerPoint.getLat() + ", " + centerPoint.getLon());
            
            // 创建CompletableFuture
            CompletableFuture<String> future = new CompletableFuture<>();
            future = future.completeOnTimeout(null, 6000, TimeUnit.MILLISECONDS);
            
            // 调用真实的周边搜索功能 - 使用MCP专用重载方法，传递排序类型和pageSize
            int taskId = SearchPackage.getInstance().aroundSearch(
                pageNum, 
                keyword, 
                centerPoint, 
                searchRadius.toString(), 
                !showUi,  // isSilentSearch = !showUi（显示UI时不静默搜索）
                showUi,   // showUI参数
                sortType,  // 排序类型
                pageSize  // 每页大小限制
            );
            
            Log.d(TAG, "周边搜索taskId: " + taskId);
            
            // 将taskId和future关联
            baseHelper.addAroundSearchTask(taskId, future);
            
            // 等待结果，最多6秒
            String searchResult = future.get(6000, TimeUnit.MILLISECONDS);
            
            // 清理任务
            baseHelper.removeAroundSearchTask(taskId);
            
            if (searchResult != null) {
                // 处理pageSize限制
                if (pageSize != null && pageSize > 0) {
                    try {
                        JsonObject resultObj = baseHelper.gson.fromJson(searchResult, JsonObject.class);
                        if (resultObj.has("results")) {
                            JsonArray results = resultObj.getAsJsonArray("results");
                            if (results.size() > pageSize) {
                                // 截取前 pageSize 个结果
                                JsonArray limitedResults = new JsonArray();
                                for (int i = 0; i < pageSize; i++) {
                                    limitedResults.add(results.get(i));
                                }
                                resultObj.add("results", limitedResults);
                                resultObj.addProperty("actual_count", results.size());
                                resultObj.addProperty("returned_count", pageSize);
                                searchResult = baseHelper.gson.toJson(resultObj);
                                Log.d(TAG, "搜索结果已截取: 原始=" + results.size() + ", 返回=" + pageSize);
                            }
                        }
                    } catch (Exception e) {
                        Log.w(TAG, "截取搜索结果失败", e);
                    }
                }
                
                // 如果显示UI，返回成功响应而不是数据
                if (showUi) {
                    JsonObject uiResponse = new JsonObject();
                    uiResponse.addProperty("status", "success");
                    uiResponse.addProperty("ui_displayed", true);
                    uiResponse.addProperty("message", "搜索结果已在地图和列表中显示");
                    uiResponse.addProperty("keyword", keyword);
                    uiResponse.addProperty("search_radius", searchRadius);
                    uiResponse.addProperty("sort_type", sortType);
                    
                    // 尝试从结果中获取实际显示数量
                    int displayedCount = 10; // 默认值
                    try {
                        JsonObject resultObj = baseHelper.gson.fromJson(searchResult, JsonObject.class);
                        if (resultObj.has("results")) {
                            displayedCount = resultObj.getAsJsonArray("results").size();
                        }
                    } catch (Exception e) {
                        // 忽略解析错误，使用默认值
                    }
                    uiResponse.addProperty("displayed_count", Math.min(displayedCount, pageSize != null ? pageSize : displayedCount));
                    
                    String uiResponseJson = baseHelper.gson.toJson(uiResponse);
                    Log.d(TAG, "UI显示成功响应: " + uiResponseJson);
                    return uiResponseJson;
                }
                
                Log.d(TAG, "周边搜索成功，返回数据结果");
                return searchResult;
            } else {
                // 搜索超时或无结果
                JsonObject noResult = new JsonObject();
                noResult.addProperty("keyword", keyword);
                noResult.addProperty("search_radius", searchRadius);
                noResult.addProperty("sort_type", sortType);
                noResult.addProperty("total_results", 0);
                noResult.add("results", baseHelper.gson.toJsonTree(new ArrayList<>()));
                noResult.addProperty("message", "周边搜索超时或无结果");
                
                String noResultJson = baseHelper.gson.toJson(noResult);
                Log.w(TAG, "周边搜索无结果: " + noResultJson);
                return noResultJson;
            }
            
        } catch (TimeoutException e) {
            Log.w(TAG, "周边搜索超时");
            JsonObject error = baseHelper.createErrorResponse("周边搜索超时，请重试", null);
            error.addProperty("keyword", keyword);
            return Prompt.text(baseHelper.gson.toJson(error));
        } catch (Exception e) {
            Log.e(TAG, "周边搜索失败", e);
            JsonObject error = baseHelper.createErrorResponse("周边搜索失败", e.getMessage());
            error.addProperty("keyword", keyword);
            return Prompt.text(baseHelper.gson.toJson(error));
        }
    }
    
}
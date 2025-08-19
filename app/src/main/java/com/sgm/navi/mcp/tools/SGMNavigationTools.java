package com.sgm.navi.mcp.tools;

import android.content.Context;
import android.content.Intent;
import android.util.Log;
import androidx.localbroadcastmanager.content.LocalBroadcastManager;

import com.sgm.navi.mcp.core.MCPDataType;
import com.sgm.navi.mcp.core.MCPParam;
import com.sgm.navi.mcp.core.MCPTool;
import com.sgm.navi.service.adapter.position.PositionAdapter;
import com.sgm.navi.service.define.position.LocInfoBean;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.search.SearchResultEntity;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.service.callback.MCPGeoSearchCallback;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.service.define.route.RouteRequestParam;
import com.sgm.navi.service.define.route.RouteParam;
import com.sgm.navi.service.define.route.RoutePoiType;
import com.sgm.navi.service.define.map.MapType;
import com.android.utils.thread.ThreadManager;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonObject;
import com.sgm.navi.service.utils.ExportIntentParam;
import com.sgm.navi.mapservice.bean.INaviConstant;
import com.sgm.navi.service.AppCache;
import com.android.utils.process.ProcessManager;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * SGM导航MCP工具类
 * 封装真实的导航功能供MCP调用
 */
public class SGMNavigationTools implements MCPGeoSearchCallback {
    private static final String TAG = "SGMNavigationTools";
    private final Gson gson = new GsonBuilder()
        .disableHtmlEscaping()
        .create();
    
    // 静态任务管理器：存储待完成的geoSearch CompletableFuture
    private static final ConcurrentHashMap<Integer, CompletableFuture<String>> pendingGeoSearchTasks = new ConcurrentHashMap<>();
    
    // 静态任务管理器：存储待完成的keywordSearch CompletableFuture
    private static final ConcurrentHashMap<Integer, CompletableFuture<String>> pendingKeywordSearchTasks = new ConcurrentHashMap<>();

    /**
     * 获取当前位置
     */
    @MCPTool(
        name = "get_current_location",
        description = "获取车辆当前完整位置信息，包括GPS坐标(经纬度)、定位精度、详细地址、行驶速度和方向等",
        returnType = MCPDataType.STRING
    )
    public String getCurrentLocation() {
        Log.d(TAG, "获取当前位置");
        
        try {
            // 调用真实的SGM定位服务
            LocInfoBean locInfo = PositionAdapter.getInstance().getLastCarLocation();
            JsonObject locationInfo = new JsonObject();

            if (locInfo != null) {
                locationInfo.addProperty("latitude", locInfo.getLatitude());
                locationInfo.addProperty("longitude", locInfo.getLongitude());
                locationInfo.addProperty("accuracy", locInfo.getAccuracy());
                
                // 改进地址信息处理逻辑
                String address = locInfo.getAddress();
                if (address != null && !address.trim().isEmpty()) {
                    locationInfo.addProperty("address", address);
                } else {
                    // 尝试通过逆地理编码获取地址
                    String geoSearchAddress = tryGetAddressByGeoSearch(locInfo.getLatitude(), locInfo.getLongitude());
                    
                    if (geoSearchAddress != null && !geoSearchAddress.trim().isEmpty()) {
                        locationInfo.addProperty("address", geoSearchAddress);
                        locationInfo.addProperty("address_source", "geo_search");
                    } else {
                        // 如果逆地理编码也失败，提供坐标信息的描述性文字
                        String coordinateDescription = String.format("GPS坐标: %.6f, %.6f", 
                            locInfo.getLatitude(), locInfo.getLongitude());
                        locationInfo.addProperty("address", coordinateDescription);
                        locationInfo.addProperty("address_note", "详细地址暂不可用，显示GPS坐标");
                    }
                }
                
                locationInfo.addProperty("speed", locInfo.getSpeed());
                locationInfo.addProperty("bearing", locInfo.getBearing());
                locationInfo.addProperty("timestamp", System.currentTimeMillis());
                locationInfo.addProperty("source", "SGM_GPS");
                
                String result = gson.toJson(locationInfo);
                Log.d(TAG, "真实位置: " + result);
                return result;
            } else {
                // Fallback: 如果真实API返回null，提供错误信息
                JsonObject error = new JsonObject();
                error.addProperty("error", "无法获取位置信息，定位服务可能未就绪");
                error.addProperty("timestamp", System.currentTimeMillis());
                
                String result = gson.toJson(error);
                Log.w(TAG, "位置获取失败: " + result);
                return result;
            }
            
        } catch (Exception e) {
            Log.e(TAG, "获取位置失败", e);
            JsonObject error = new JsonObject();
            error.addProperty("error", "获取位置失败: " + e.getMessage());
            return gson.toJson(error);
        }
    }

    /**
     * 尝试通过逆地理搜索获取地址
     * 使用CompletableFuture将异步结果转为同步
     * 
     * @param latitude 纬度
     * @param longitude 经度
     * @return 地址信息，获取失败或超时返回null
     */
    private String tryGetAddressByGeoSearch(double latitude, double longitude) {
        try {
            Log.d(TAG, "开始逆地理搜索，坐标: " + latitude + ", " + longitude);
            
            // 创建CompletableFuture
            CompletableFuture<String> future = new CompletableFuture<>();
            
            // 设置3秒超时，超时返回null
            future = future.completeOnTimeout(null, 3000, TimeUnit.MILLISECONDS);
            
            // 创建GeoPoint
            GeoPoint geoPoint = new GeoPoint();
            geoPoint.setLat(latitude);
            geoPoint.setLon(longitude);
            
            // 发起静默逆地理搜索，获取taskId
            int taskId = SearchPackage.getInstance().geoSearch(geoPoint, true); // 使用静默搜索
            Log.d(TAG, "逆地理搜索taskId: " + taskId);
            
            // 将taskId和future关联
            pendingGeoSearchTasks.put(taskId, future);
            
            // 等待结果，最多3秒
            String result = future.get(3000, TimeUnit.MILLISECONDS);
            
            // 清理任务
            pendingGeoSearchTasks.remove(taskId);
            
            Log.d(TAG, "逆地理搜索结果: " + result);
            return result;
            
        } catch (TimeoutException e) {
            Log.w(TAG, "逆地理搜索超时");
            return null;
        } catch (Exception e) {
            Log.w(TAG, "逆地理搜索失败: " + e.getMessage());
            return null;
        }
    }

    /**
     * 搜索地点(POI)
     */
    @MCPTool(
        name = "search_poi",
        description = "搜索地点(POI)，如餐厅、商场、景点等，返回详细的搜索结果列表",
        returnType = MCPDataType.STRING
    )
    public String searchPoi(
            @MCPParam(name = "keyword", type = MCPDataType.STRING,
                      description = "搜索关键词，如'星巴克'、'上海迪士尼'、'加油站'", required = true)
            String keyword,
            @MCPParam(name = "max_results", type = MCPDataType.NUMBER,
                      description = "最大返回结果数，默认5个，最大10个", required = false)
            String maxResults) {
        
        Log.d(TAG, "搜索POI: " + keyword);
        
        try {
            // 处理最大结果数参数
            int maxResultsNum = 5; // 默认值
            if (maxResults != null && !maxResults.trim().isEmpty()) {
                try {
                    maxResultsNum = Integer.parseInt(maxResults.trim());
                    // 限制在1-10之间
                    maxResultsNum = Math.max(1, Math.min(10, maxResultsNum));
                } catch (NumberFormatException e) {
                    Log.w(TAG, "无效的最大结果数: " + maxResults + ", 使用默认值5");
                }
            }
            
            Log.d(TAG, "开始关键词搜索，关键词: " + keyword + ", 最大结果数: " + maxResultsNum);
            
            // 创建CompletableFuture
            CompletableFuture<String> future = new CompletableFuture<>();
            
            // 设置超时，超时返回null
            future = future.completeOnTimeout(null, 8000, TimeUnit.MILLISECONDS);
            
            // 发起关键词搜索，使用静默搜索避免影响UI
            int taskId = SearchPackage.getInstance().keywordSearch(1, keyword, true);
            Log.d(TAG, "关键词搜索taskId: " + taskId);
            
            // 将taskId和future关联，同时存储搜索参数
            pendingKeywordSearchTasks.put(taskId, future);
            
            // 等待结果，最多5秒
            String result = future.get(8000, TimeUnit.MILLISECONDS);
            
            // 清理任务
            pendingKeywordSearchTasks.remove(taskId);
            
            if (result != null) {
                Log.d(TAG, "关键词搜索成功: " + result);
                return result;
            } else {
                // 搜索超时或无结果
                JsonObject noResult = new JsonObject();
                noResult.addProperty("keyword", keyword);
                noResult.addProperty("total_results", 0);
                noResult.add("results", gson.toJsonTree(new ArrayList<>()));
                noResult.addProperty("message", "搜索超时或无结果");
                
                String noResultJson = gson.toJson(noResult);
                Log.w(TAG, "关键词搜索无结果: " + noResultJson);
                return noResultJson;
            }
            
        } catch (TimeoutException e) {
            Log.w(TAG, "关键词搜索超时");
            JsonObject error = new JsonObject();
            error.addProperty("error", "搜索超时，请重试");
            error.addProperty("keyword", keyword);
            return gson.toJson(error);
        } catch (Exception e) {
            Log.e(TAG, "关键词搜索失败", e);
            JsonObject error = new JsonObject();
            error.addProperty("error", "搜索失败: " + e.getMessage());
            error.addProperty("keyword", keyword);
            return gson.toJson(error);
        }
    }

    /**
     * 启动导航到指定坐标点
     */
    @MCPTool(
        name = "start_navigation",
        description = "启动导航到指定坐标点，需要先使用search_poi获取POI的坐标,然后将位置名称和坐标作为参数再传进来",
        returnType = MCPDataType.STRING
    )
    public String startNavigation(
            @MCPParam(name = "latitude", type = MCPDataType.NUMBER,
                      description = "目的地纬度", required = true)
            String latitude,
            @MCPParam(name = "longitude", type = MCPDataType.NUMBER,
                      description = "目的地经度", required = true)
            String longitude,
            @MCPParam(name = "poi_name", type = MCPDataType.STRING,
                      description = "目的地POI名称", required = false)
            String poiName,
            @MCPParam(name = "address", type = MCPDataType.STRING,
                      description = "目的地地址", required = false)
            String address,
            @MCPParam(name = "simulate", type = MCPDataType.STRING,
                      description = "是否模拟导航: true/false，默认false", required = false)
            String simulate) {
        
        Log.d(TAG, "开始导航到坐标: " + latitude + ", " + longitude + ", POI名称: " + poiName);
        Log.d(TAG, "POI名称字节数组: " + (poiName != null ? java.util.Arrays.toString(poiName.getBytes(java.nio.charset.StandardCharsets.UTF_8)) : "null"));
        
        // 尝试修复编码问题
        if (poiName != null) {
            poiName = fixEncoding(poiName);
            Log.d(TAG, "编码修复后POI名称: " + poiName);
        }
        
        // 检查必要参数是否存在
        if (latitude == null || latitude.trim().isEmpty() || 
            longitude == null || longitude.trim().isEmpty()) {
            Log.e(TAG, "缺少必要的坐标参数 - latitude: " + latitude + ", longitude: " + longitude);
            
            JsonObject error = new JsonObject();
            error.addProperty("status", "error");
            error.addProperty("error", "参数格式错误");
            error.addProperty("message", "请直接提供latitude和longitude参数，不要嵌套在其他对象中");
            
            // 添加正确的参数格式示例
            JsonObject expectedFormat = new JsonObject();
            expectedFormat.addProperty("latitude", "31.215725");
            expectedFormat.addProperty("longitude", "121.552563");
            expectedFormat.addProperty("poi_name", "目的地名称(可选)");
            expectedFormat.addProperty("address", "详细地址(可选)");
            expectedFormat.addProperty("simulate", "false(可选)");
            
            error.add("expected_format", expectedFormat);
            error.addProperty("hint", "请使用扁平化的参数结构，例如: {\"latitude\": \"31.215725\", \"longitude\": \"121.552563\"}");
            error.addProperty("invalid_format_example", "错误格式: {\"destination\": {\"latitude\": ..., \"longitude\": ...}}");
            
            return gson.toJson(error);
        }
        
        try {
            // 解析坐标参数
            double lat = Double.parseDouble(latitude);
            double lng = Double.parseDouble(longitude);
            boolean isSimulate = "true".equalsIgnoreCase(simulate);
            
            Log.d(TAG, "使用广播机制启动导航...");
            
            // 创建目的地POI信息
            PoiInfoEntity destinationPoi = new PoiInfoEntity();
            destinationPoi.setName(poiName != null ? poiName : "目的地");
            destinationPoi.setAddress(address != null ? address : "");
            
            GeoPoint destPoint = new GeoPoint();
            destPoint.setLat(lat);
            destPoint.setLon(lng);
            destinationPoi.setPoint(destPoint);
            
            // 发起路线规划
            RouteRequestParam routeRequest = new RouteRequestParam();
            routeRequest.setMMapTypeId(MapType.MAIN_SCREEN_MAIN_MAP);
            routeRequest.setMPoiInfoEntity(destinationPoi);
            routeRequest.setMRoutePoiType(RoutePoiType.ROUTE_POI_TYPE_END); // 终点
            routeRequest.setMIsOnline(true); // 在线路线规划
            
            Log.d(TAG, "发起路线规划请求...");
            long routeTaskId = RoutePackage.getInstance().requestRoute(routeRequest);
            
            if (routeTaskId > 0) {
                Log.d(TAG, "路线规划请求成功，taskId: " + routeTaskId);
                
                // 使用广播通知MapActivity处理导航
                Context context = AppCache.getInstance().getMContext();
                if (context != null) {
                    Intent intent = new Intent("com.sgm.navi.ACTION_START_NAVIGATION");
                    intent.putExtra("route_task_id", routeTaskId);
                    intent.putExtra("latitude", lat);
                    intent.putExtra("longitude", lng);
                    intent.putExtra("poi_name", poiName);
                    intent.putExtra("address", address);
                    intent.putExtra("simulate", isSimulate);
                    intent.putExtra("auto_start", true); // 自动开始导航标志
                    
                    // 使用LocalBroadcastManager发送广播
                    LocalBroadcastManager.getInstance(context).sendBroadcast(intent);
                    Log.d(TAG, "已发送本地导航广播");
                    
                    // 同时启动MapActivity（如果不在前台）
                    if (!isMapActivityInForeground()) {
                        Intent activityIntent = new Intent();
                        activityIntent.setClassName(context.getPackageName(), 
                            context.getPackageName() + ".map.MapActivity");
                        activityIntent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_CLEAR_TOP);
                        activityIntent.putExtras(intent.getExtras());
                        
                        try {
                            context.startActivity(activityIntent);
                            Log.d(TAG, "已启动MapActivity");
                        } catch (Exception e) {
                            Log.e(TAG, "启动MapActivity失败: " + e.getMessage());
                        }
                    } else {
                        Log.d(TAG, "MapActivity已在前台，仅发送广播");
                    }
                    
                } else {
                    Log.w(TAG, "应用上下文不可用，使用回调机制");
                    NaviPackage.getInstance().onMCPRequestNavigation(lat, lng, poiName, address, isSimulate);
                }
            } else {
                Log.e(TAG, "路线规划请求失败");
                JsonObject error = new JsonObject();
                error.addProperty("status", "error");
                error.addProperty("error", "路线规划请求失败");
                error.addProperty("message", "无法发起路线规划，请检查网络连接");
                return gson.toJson(error);
            }
            
            // 创建返回结果
            JsonObject navResult = new JsonObject();
            navResult.addProperty("status", "success");
            navResult.addProperty("message", "导航请求已发送");
            navResult.addProperty("route_task_id", routeTaskId);
            navResult.addProperty("destination_name", poiName != null ? poiName : "目的地");
            navResult.addProperty("destination_address", address != null ? address : "");
            navResult.addProperty("latitude", lat);
            navResult.addProperty("longitude", lng);
            navResult.addProperty("simulate", isSimulate);
            
            String result = gson.toJson(navResult);
            Log.d(TAG, "导航启动结果: " + result);
            return result;
            
        } catch (NumberFormatException e) {
            Log.e(TAG, "坐标数值格式错误", e);
            JsonObject error = new JsonObject();
            error.addProperty("status", "error");
            error.addProperty("error", "坐标数值格式错误: " + e.getMessage());
            error.addProperty("message", "请提供有效的数字格式的经纬度坐标");
            return gson.toJson(error);
        } catch (Exception e) {
            Log.e(TAG, "导航启动失败", e);
            JsonObject error = new JsonObject();
            error.addProperty("status", "error");
            error.addProperty("error", "导航启动失败: " + e.getMessage());
            return gson.toJson(error);
        }
    }

    /**
     * 搜索附近的兴趣点
     */
    @MCPTool(
        name = "search_nearby",
        description = "搜索附近的兴趣点(POI)",
        returnType = MCPDataType.STRING
    )
    public String searchNearby(
            @MCPParam(name = "keyword", type = MCPDataType.STRING,
                      description = "搜索关键词，如：加油站、餐厅、停车场", required = true)
            String keyword,
            @MCPParam(name = "radius", type = MCPDataType.NUMBER,
                      description = "搜索半径(米)，默认5000米", required = false)
            String radius) {
        
        Log.d(TAG, "搜索附近: " + keyword + ", 半径: " + radius + "米");
        
        try {
            // TODO: 集成真实的POI搜索服务
            // 这里应该调用SGM地图的真实POI搜索API
            // List<POI> pois = SGMMapService.getInstance().searchNearby(keyword, searchRadius);
            
            int searchRadius = 5000;
            try {
                if (radius != null && !radius.isEmpty()) {
                    searchRadius = Integer.parseInt(radius);
                }
            } catch (NumberFormatException e) {
                Log.w(TAG, "无效的搜索半径: " + radius);
            }
            
            // 模拟POI搜索结果
            List<JsonObject> pois = generateMockPOIs(keyword);
            
            JsonObject searchResult = new JsonObject();
            searchResult.addProperty("keyword", keyword);
            searchResult.addProperty("radius", searchRadius);
            searchResult.addProperty("count", pois.size());
            searchResult.add("pois", gson.toJsonTree(pois));
            
            String result = gson.toJson(searchResult);
            Log.d(TAG, "附近搜索结果: " + result);
            return result;
            
        } catch (Exception e) {
            Log.e(TAG, "附近搜索失败", e);
            JsonObject error = new JsonObject();
            error.addProperty("error", "附近搜索失败: " + e.getMessage());
            return gson.toJson(error);
        }
    }

    /**
     * 生成模拟POI数据
     */
    private List<JsonObject> generateMockPOIs(String keyword) {
        List<JsonObject> pois = new ArrayList<>();
        
        switch (keyword) {
            case "加油站":
                pois.add(createPOI("中石化加油站(世纪大道店)", "31.240706", "121.500809", "500"));
                pois.add(createPOI("中石油加油站(浦东南路店)", "31.242706", "121.502809", "1200"));
                pois.add(createPOI("壳牌加油站(张杨路店)", "31.245706", "121.505809", "2300"));
                break;
                
            case "停车场":
                pois.add(createPOI("东方明珠地下停车场", "31.239906", "121.499609", "200"));
                pois.add(createPOI("国金中心停车场", "31.241706", "121.501809", "800"));
                pois.add(createPOI("正大广场停车场", "31.243706", "121.503809", "1500"));
                break;
                
            case "餐厅":
                pois.add(createPOI("外滩茂悦大酒店餐厅", "31.240406", "121.500409", "300"));
                pois.add(createPOI("小南国(陆家嘴店)", "31.241706", "121.502409", "600"));
                pois.add(createPOI("海底捞(世纪大道店)", "31.242706", "121.503409", "900"));
                pois.add(createPOI("星巴克(东方明珠店)", "31.239806", "121.499909", "100"));
                break;
                
            default:
                pois.add(createPOI(keyword + "1号店", "31.240706", "121.500809", "800"));
                pois.add(createPOI(keyword + "2号店", "31.242706", "121.502809", "1500"));
                break;
        }
        
        return pois;
    }

    private JsonObject createPOI(String name, String latitude, String longitude, String distance) {
        JsonObject poi = new JsonObject();
        poi.addProperty("name", name);
        poi.addProperty("latitude", Double.parseDouble(latitude));
        poi.addProperty("longitude", Double.parseDouble(longitude));
        poi.addProperty("distance", Integer.parseInt(distance));
        poi.addProperty("unit", "米");
        return poi;
    }
    
    /**
     * 完成geoSearch任务 - 实现MCPGeoSearchCallback接口
     * 
     * @param taskId 任务ID
     * @param address 解析得到的地址，可以为null
     */
    @Override
    public void completeGeoSearchTask(int taskId, String address) {
        CompletableFuture<String> future = pendingGeoSearchTasks.remove(taskId);
        if (future != null && !future.isDone()) {
            future.complete(address);
            Log.d(TAG, "geoSearch任务完成: taskId=" + taskId + ", address=" + address);
        }
    }
    
    /**
     * 从搜索结果中提取地址信息 - 实现MCPGeoSearchCallback接口
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
                
                Log.d(TAG, "提取地址信息: " + address);
                return address;
            }
        } catch (Exception e) {
            Log.w(TAG, "提取地址信息失败: " + e.getMessage());
        }
        return null;
    }
    
    /**
     * 完成关键词搜索任务 - 实现MCPGeoSearchCallback接口
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
     * 从搜索结果中提取POI列表并格式化为JSON - 实现MCPGeoSearchCallback接口
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
                LocInfoBean currentLocation = null;
                try {
                    currentLocation = PositionAdapter.getInstance().getLastCarLocation();
                } catch (Exception e) {
                    Log.w(TAG, "获取当前位置失败: " + e.getMessage());
                }
                
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
     * 检查MapActivity是否在前台
     * 通过反射获取MapActivity的静态变量isInForeground
     */
    private boolean isMapActivityInForeground() {
        try {
            // 使用反射获取MapActivity的isInForeground状态
            Class<?> mapActivityClass = Class.forName("com.sgm.navi.hmi.map.MapActivity");
            java.lang.reflect.Field field = mapActivityClass.getField("isInForeground");
            return field.getBoolean(null);
        } catch (Exception e) {
            Log.w(TAG, "检查MapActivity前台状态失败: " + e.getMessage());
            // 如果获取失败，返回false，让Activity启动逻辑执行
            return false;
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
    
    /**
     * 修复字符串编码问题
     * 尝试检测和修复可能的UTF-8编码问题
     */
    private String fixEncoding(String input) {
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
}
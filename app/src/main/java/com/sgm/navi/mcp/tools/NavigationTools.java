package com.sgm.navi.mcp.tools;

import android.content.Context;
import android.content.Intent;
import android.util.Log;
import androidx.localbroadcastmanager.content.LocalBroadcastManager;

import com.google.gson.JsonObject;
// 原始MCP注解
import com.sgm.navi.mcp.core.MCPDataType;
import com.sgm.navi.mcp.core.MCPParam;
import com.sgm.navi.mcp.core.MCPTool;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.route.RouteRequestParam;
import com.sgm.navi.service.define.route.RoutePoiType;
import com.sgm.navi.service.define.route.RoutePreferenceID;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.define.navi.NaviEtaInfo;
import com.sgm.navi.service.AppCache;
import com.android.utils.process.ProcessManager;
import com.sgm.navi.service.logicpaket.setting.SettingPackage;

/**
 * 导航相关MCP工具类
 * 负责处理路线规划、导航启动等导航相关功能
 */
public class NavigationTools {
    private static final String TAG = "NavigationTools";
    private final BaseToolHelper baseHelper = BaseToolHelper.getInstance();

    /**
     * 启动导航到指定坐标点
     */
    @MCPTool(
        name = "start_navigation",
        title = "导航启动工具",
        description = "规划路线到指定坐标点，路线规划完成后会显示路线供用户查看，然后按正常导航流程开始导航。需要先使用search_poi获取POI的坐标,然后将位置名称和坐标作为参数再传进来。调用示例：{\"latitude\":\"31.230390\",\"longitude\":\"121.473700\",\"poi_name\":\"上海迪士尼\",\"address\":\"上海市浦东新区川沙镇\",\"route_preference\":\"avoid_congestion\",\"avoid_limit\":\"true\",\"simulate\":\"false\"}",
        returnDescription = "返回导航启动结果，包含路线规划状态和导航信息",
        openWorldHint = true
    )
    public String startNavigation(
            @MCPParam(name = "latitude",
                      description = "目的地纬度。格式：数字字符串。示例：\"31.230390\"、\"39.904200\"。注意：必须是字符串格式的数字，带引号", 
                      required = true)
            String latitude,
            @MCPParam(name = "longitude",
                      description = "目的地经度。格式：数字字符串。示例：\"121.473700\"、\"116.407400\"。注意：必须是字符串格式的数字，带引号", 
                      required = true)
            String longitude,
            @MCPParam(name = "poi_name",
                      description = "目的地POI名称。格式：字符串类型。示例：\"上海迪士尼\"、\"北京天安门\"。可选参数，有助于导航识别", 
                      required = false)
            String poiName,
            @MCPParam(name = "address",
                      description = "目的地地址。格式：字符串类型。示例：\"上海市浦东新区川沙镇\"、\"北京市东城区\"。可选参数，提供详细地址信息", 
                      required = false)
            String address,
            @MCPParam(name = "route_preference",
                      description = "路线偏好设置。可选值：\"recommend\"(推荐)、\"avoid_congestion\"(躲避拥堵)、\"less_charge\"(少收费)、\"no_highway\"(不走高速)、\"highway_first\"(高速优先)、\"fastest_speed\"(速度最快)。默认使用系统设置", 
                      required = false)
            String routePreference,
            @MCPParam(name = "avoid_limit",
                      description = "是否避开限行。格式：字符串类型。示例：\"true\"、\"false\"。默认使用系统设置。需要用户已设置车牌号", 
                      required = false)
            String avoidLimit,
            @MCPParam(name = "simulate",
                      description = "是否模拟导航。格式：字符串类型。示例：\"true\"、\"false\"。默认\"false\"。注意：必须是字符串，不要传入布尔值", 
                      required = false)
            String simulate) {
        
        Log.d(TAG, "开始导航到坐标: " + latitude + ", " + longitude + ", POI名称: " + poiName);
        Log.d(TAG, "POI名称字节数组: " + (poiName != null ? java.util.Arrays.toString(poiName.getBytes(java.nio.charset.StandardCharsets.UTF_8)) : "null"));
        Log.d(TAG, "导航参数 - 路线偏好: " + routePreference + ", 避开限行: " + avoidLimit + ", 模拟: " + simulate);
        
        // 处理避开限行参数
        if (avoidLimit != null && !avoidLimit.trim().isEmpty()) {
            boolean shouldAvoidLimit = "true".equalsIgnoreCase(avoidLimit);
            
            if (shouldAvoidLimit) {
                // 检查车牌是否已设置
                String plateNumber = SettingPackage.getInstance().getConfigKeyPlateNumber();
                if (plateNumber == null || plateNumber.trim().isEmpty()) {
                    Log.e(TAG, "避开限行需要先设置车牌号码");
                    
                    JsonObject error = baseHelper.createErrorResponse(
                        "限行功能需要车牌", 
                        "请先设置车牌号码才能使用避开限行功能"
                    );
                    error.addProperty("error_code", "NO_PLATE_NUMBER");
                    error.addProperty("suggestion", "请先调用set_plate_number设置车牌");
                    
                    return baseHelper.gson.toJson(error);
                }
                Log.d(TAG, "已检测到车牌号码: " + plateNumber + "，将应用避开限行");
            }
        }
        
        // 尝试修复编码问题
        if (poiName != null) {
            poiName = baseHelper.fixEncoding(poiName);
            Log.d(TAG, "编码修复后POI名称: " + poiName);
        }
        
        // 检查必要参数是否存在
        if (latitude == null || latitude.trim().isEmpty() || 
            longitude == null || longitude.trim().isEmpty()) {
            Log.e(TAG, "缺少必要的坐标参数 - latitude: " + latitude + ", longitude: " + longitude);
            
            JsonObject error = baseHelper.createErrorResponse("参数格式错误", "请直接提供latitude和longitude参数，不要嵌套在其他对象中");
            
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
            
            return baseHelper.gson.toJson(error);
        }
        
        try {
            // 解析坐标参数
            double lat = Double.parseDouble(latitude);
            double lng = Double.parseDouble(longitude);
            boolean isSimulate = "true".equalsIgnoreCase(simulate);
            
            // 1. 检查当前导航状态
            String currentNaviStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
            Log.d(TAG, "当前导航状态: " + currentNaviStatus);
            
            // 2. 如果正在导航，先停止当前导航
            if (NaviStatus.NaviStatusType.NAVING.equals(currentNaviStatus) || 
                NaviStatus.NaviStatusType.LIGHT_NAVING.equals(currentNaviStatus)) {
                Log.d(TAG, "检测到正在导航，先停止当前导航");
                
                boolean stopResult = NaviPackage.getInstance().stopNavigation(true);
                Log.d(TAG, "停止导航结果: " + stopResult);
                
                if (!stopResult) {
                    Log.w(TAG, "停止当前导航失败，但继续执行新导航");
                }
                
                // 等待导航状态稳定
                try {
                    Thread.sleep(800);
                    Log.d(TAG, "等待导航状态稳定完成");
                } catch (InterruptedException e) {
                    Log.w(TAG, "等待导航状态稳定被中断: " + e.getMessage());
                    Thread.currentThread().interrupt();
                }
            }
            
            // 3. 导航前清理之前的搜索标记，避免图层叠加
            try {
                SearchPackage.getInstance().clearLabelMark();
                Log.d(TAG, "已清理之前的搜索标记");
            } catch (Exception e) {
                Log.w(TAG, "清理搜索标记失败: " + e.getMessage());
            }
            
            Log.d(TAG, "使用广播机制启动导航...");
            
            // 创建目的地POI信息
            PoiInfoEntity destinationPoi = new PoiInfoEntity();
            destinationPoi.setName(poiName != null ? poiName : "目的地");
            destinationPoi.setAddress(address != null ? address : "");
            
            GeoPoint destPoint = new GeoPoint();
            destPoint.setLat(lat);
            destPoint.setLon(lng);
            destinationPoi.setPoint(destPoint);
            
            // 解析路线偏好参数
            RoutePreferenceID preferenceId = null;
            if (routePreference != null && !routePreference.trim().isEmpty()) {
                try {
                    switch (routePreference.toLowerCase().trim()) {
                        case "recommend":
                            preferenceId = RoutePreferenceID.PREFERENCE_RECOMMEND;
                            break;
                        case "avoid_congestion":
                            preferenceId = RoutePreferenceID.PREFERENCE_AVOIDCONGESTION;
                            break;
                        case "less_charge":
                            preferenceId = RoutePreferenceID.PREFERENCE_LESSCHARGE;
                            break;
                        case "no_highway":
                            preferenceId = RoutePreferenceID.PREFERENCE_NOTHIGHWAY;
                            break;
                        case "highway_first":
                            preferenceId = RoutePreferenceID.PREFERENCE_FIRSTHIGHWAY;
                            break;
                        case "fastest_speed":
                            preferenceId = RoutePreferenceID.PREFERENCE_FASTESTSPEED;
                            break;
                        default:
                            Log.w(TAG, "未知的路线偏好参数: " + routePreference + "，使用系统默认设置");
                            break;
                    }
                } catch (Exception e) {
                    Log.e(TAG, "解析路线偏好参数失败: " + e.getMessage());
                }
            }
            
            Log.d(TAG, "路线偏好设置: " + (preferenceId != null ? preferenceId.name() : "使用系统默认"));
            
            // 发起路线规划
            RouteRequestParam routeRequest = new RouteRequestParam();
            routeRequest.setMMapTypeId(MapType.MAIN_SCREEN_MAIN_MAP);
            routeRequest.setMPoiInfoEntity(destinationPoi);
            routeRequest.setMRoutePoiType(RoutePoiType.ROUTE_POI_TYPE_END); // 终点
            routeRequest.setMIsOnline(true); // 在线路线规划
            
            // 设置路线偏好
            if (preferenceId != null) {
                routeRequest.setMRoutePreferenceID(preferenceId);
                Log.d(TAG, "应用路线偏好: " + preferenceId.name());
            }
            
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
                    intent.putExtra("route_preference", routePreference);
                    intent.putExtra("avoid_limit", avoidLimit);
                    intent.putExtra("simulate", isSimulate);
                    intent.putExtra("auto_start", false); // 自动开始导航标志
                    
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
                JsonObject error = baseHelper.createErrorResponse("路线规划请求失败", "无法发起路线规划，请检查网络连接");
                return baseHelper.gson.toJson(error);
            }
            
            // 创建返回结果
            JsonObject navResult = baseHelper.createSuccessResponse();
            
            // 根据之前是否在导航中来设置消息
            String message = NaviStatus.NaviStatusType.NAVING.equals(currentNaviStatus) || 
                            NaviStatus.NaviStatusType.LIGHT_NAVING.equals(currentNaviStatus) ?
                            "已停止当前导航，路线规划完成后将显示路线" : "路线规划完成后将显示路线";
                            
            navResult.addProperty("message", message);
            navResult.addProperty("route_task_id", routeTaskId);
            navResult.addProperty("destination_name", poiName != null ? poiName : "目的地");
            navResult.addProperty("destination_address", address != null ? address : "");
            navResult.addProperty("latitude", lat);
            navResult.addProperty("longitude", lng);
            navResult.addProperty("route_preference", routePreference != null ? routePreference : "系统默认");
            navResult.addProperty("avoid_limit", avoidLimit != null ? avoidLimit : "系统默认");
            navResult.addProperty("simulate", isSimulate);
            navResult.addProperty("previous_navigation_stopped", 
                NaviStatus.NaviStatusType.NAVING.equals(currentNaviStatus) || 
                NaviStatus.NaviStatusType.LIGHT_NAVING.equals(currentNaviStatus));
            navResult.addProperty("previous_navigation_status", currentNaviStatus);
            
            String result = baseHelper.gson.toJson(navResult);
            Log.d(TAG, "导航启动结果: " + result);
            return result;
            
        } catch (NumberFormatException e) {
            Log.e(TAG, "坐标数值格式错误", e);
            JsonObject error = baseHelper.createErrorResponse("坐标数值格式错误", "请提供有效的数字格式的经纬度坐标");
            return baseHelper.gson.toJson(error);
        } catch (Exception e) {
            Log.e(TAG, "导航启动失败", e);
            JsonObject error = baseHelper.createErrorResponse("导航启动失败", e.getMessage());
            return baseHelper.gson.toJson(error);
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
     * 获取当前导航状态概览
     */
    @MCPTool(
        name = "get_navigation_status",
        title = "导航状态查询工具",
        description = "获取当前导航状态，包括是否正在导航、导航模式、当前位置等信息",
        returnDescription = "返回导航状态信息，包含导航活动状态、当前位置等",
        readOnlyHint = true
    )
    public String getNavigationStatus() {
        Log.d(TAG, "获取导航状态");
        
        try {
            // 获取导航状态信息
            boolean isNavigationActive = NaviStatusPackage.getInstance().isGuidanceActive();
            String currentNaviStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
            
            // 获取当前位置
            GeoPoint currentLocation = null;
            try {
                currentLocation = SearchPackage.getInstance().getCurrentLocation();
            } catch (Exception e) {
                Log.w(TAG, "获取当前位置失败: " + e.getMessage());
            }
            
            // 创建结果对象
            JsonObject result = baseHelper.createSuccessResponse();
            result.addProperty("navigation_active", isNavigationActive);
            result.addProperty("navigation_status", currentNaviStatus != null ? currentNaviStatus : "UNKNOWN");
            
            // 添加当前位置信息
            if (currentLocation != null) {
                result.addProperty("current_latitude", currentLocation.getLat());
                result.addProperty("current_longitude", currentLocation.getLon());
                result.addProperty("has_location", true);
            } else {
                result.addProperty("has_location", false);
            }
            
            // 添加状态描述
            String statusMessage;
            if (isNavigationActive) {
                statusMessage = "正在导航中";
            } else if ("IDLE".equals(currentNaviStatus)) {
                statusMessage = "导航空闲，可以开始新的导航";
            } else if ("CALCULATING".equals(currentNaviStatus)) {
                statusMessage = "正在规划路线";
            } else {
                statusMessage = "导航状态: " + (currentNaviStatus != null ? currentNaviStatus : "未知");
            }
            result.addProperty("message", statusMessage);
            
            Log.d(TAG, "导航状态获取成功: " + statusMessage);
            return baseHelper.gson.toJson(result);
            
        } catch (Exception e) {
            Log.e(TAG, "获取导航状态失败", e);
            JsonObject error = baseHelper.createErrorResponse("获取失败", e.getMessage());
            return baseHelper.gson.toJson(error);
        }
    }

    /**
     * 获取详细的导航信息
     */
    @MCPTool(
        name = "get_navigation_details",
        title = "导航详细信息工具",
        description = "获取详细的导航信息，包括剩余时间、距离、当前道路等。仅在导航中有效",
        returnDescription = "返回详细导航信息，如剩余时间、距离、当前道路名称等",
        readOnlyHint = true
    )
    public String getNavigationDetails() {
        Log.d(TAG, "获取详细导航信息");
        
        try {
            // 首先检查是否正在导航
            boolean isNavigationActive = NaviStatusPackage.getInstance().isGuidanceActive();
            
            if (!isNavigationActive) {
                JsonObject result = baseHelper.createErrorResponse(
                    "当前未在导航", 
                    "请先开始导航后再查询详细导航信息"
                );
                result.addProperty("suggestion", "可以先调用get_navigation_status查看导航状态");
                return baseHelper.gson.toJson(result);
            }
            
            // 获取详细导航信息
            NaviEtaInfo naviEtaInfo = null;
            try {
                naviEtaInfo = NaviPackage.getInstance().getCurrentNaviEtaInfo();
            } catch (Exception e) {
                Log.w(TAG, "获取导航详细信息失败: " + e.getMessage());
            }
            
            if (naviEtaInfo == null) {
                JsonObject result = baseHelper.createErrorResponse(
                    "导航信息不可用", 
                    "无法获取当前导航的详细信息"
                );
                return baseHelper.gson.toJson(result);
            }
            
            // 创建结果对象
            JsonObject result = baseHelper.createSuccessResponse();
            
            // 基本导航信息
            result.addProperty("remain_time_seconds", naviEtaInfo.getRemainTime());
            result.addProperty("remain_distance_meters", naviEtaInfo.getRemainDist());
            result.addProperty("drive_time_seconds", naviEtaInfo.getDriveTime());
            result.addProperty("drive_distance_meters", naviEtaInfo.getDriveDist());
            
            // 道路信息
            result.addProperty("current_road", naviEtaInfo.getCurRouteName() != null ? naviEtaInfo.getCurRouteName() : "");
            result.addProperty("next_road", naviEtaInfo.getNextRouteName() != null ? naviEtaInfo.getNextRouteName() : "");
            result.addProperty("next_distance_meters", naviEtaInfo.getNextDist());
            
            // 导航类型
            int navType = naviEtaInfo.getType();
            String navigationType;
            switch (navType) {
                case 0: navigationType = "真实导航"; break;
                case 1: navigationType = "模拟导航"; break;
                case 2: navigationType = "巡航模式"; break;
                default: navigationType = "未知类型(" + navType + ")"; break;
            }
            result.addProperty("navigation_type", navigationType);
            
            // 格式化时间和距离信息
            result.addProperty("remain_time_formatted", formatTime(naviEtaInfo.getRemainTime()));
            result.addProperty("remain_distance_formatted", formatDistance(naviEtaInfo.getRemainDist()));
            result.addProperty("drive_time_formatted", formatTime(naviEtaInfo.getDriveTime()));
            result.addProperty("drive_distance_formatted", formatDistance(naviEtaInfo.getDriveDist()));
            
            // 预计到达时间（当前时间 + 剩余时间）
            long currentTimeMillis = System.currentTimeMillis();
            long etaMillis = currentTimeMillis + (naviEtaInfo.getRemainTime() * 1000L);
            result.addProperty("eta_timestamp", etaMillis);
            result.addProperty("eta_formatted", formatTimestamp(etaMillis));
            
            // 其他详细信息
            result.addProperty("current_speed_kmh", naviEtaInfo.getCurLinkSpeed());
            result.addProperty("path_id", naviEtaInfo.getPathID());
            result.addProperty("route_remain_light_count", naviEtaInfo.getRouteRemainLightCount());
            
            // 状态消息
            StringBuilder messageBuilder = new StringBuilder();
            messageBuilder.append("距离目的地还有").append(formatDistance(naviEtaInfo.getRemainDist()))
                         .append("，预计").append(formatTime(naviEtaInfo.getRemainTime()))
                         .append("到达");
            if (naviEtaInfo.getCurRouteName() != null && !naviEtaInfo.getCurRouteName().isEmpty()) {
                messageBuilder.append("，当前在").append(naviEtaInfo.getCurRouteName());
            }
            result.addProperty("message", messageBuilder.toString());
            
            Log.d(TAG, "导航详细信息获取成功");
            return baseHelper.gson.toJson(result);
            
        } catch (Exception e) {
            Log.e(TAG, "获取导航详细信息失败", e);
            JsonObject error = baseHelper.createErrorResponse("获取失败", e.getMessage());
            return baseHelper.gson.toJson(error);
        }
    }
    
    /**
     * 格式化时间（秒）为可读格式
     */
    private String formatTime(int seconds) {
        if (seconds <= 0) return "0分钟";
        
        int hours = seconds / 3600;
        int minutes = (seconds % 3600) / 60;
        
        if (hours > 0) {
            return hours + "小时" + minutes + "分钟";
        } else {
            return minutes + "分钟";
        }
    }
    
    /**
     * 格式化距离（米）为可读格式
     */
    private String formatDistance(int meters) {
        if (meters <= 0) return "0米";
        
        if (meters >= 1000) {
            double km = meters / 1000.0;
            return String.format("%.1f公里", km);
        } else {
            return meters + "米";
        }
    }
    
    /**
     * 格式化时间戳为可读格式
     */
    private String formatTimestamp(long timestamp) {
        try {
            java.text.SimpleDateFormat sdf = new java.text.SimpleDateFormat("HH:mm");
            return sdf.format(new java.util.Date(timestamp));
        } catch (Exception e) {
            return "时间格式错误";
        }
    }
}
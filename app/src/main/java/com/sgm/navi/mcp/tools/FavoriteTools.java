package com.sgm.navi.mcp.tools;

import android.os.Bundle;
import android.text.TextUtils;
import android.util.Log;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
// 新MCP SDK注解
import com.sgm.mcp.protocol.annotations.McpTool;
import com.sgm.mcp.protocol.annotations.McpParam;
import com.sgm.mcp.protocol.McpSpec;
import com.sgm.mcp.protocol.Prompt;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.route.RouteSpeechRequestParam;
import com.sgm.navi.service.define.search.FavoriteInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.vrbridge.IVrBridgeConstant;

import com.google.gson.JsonObject;

/**
 * 收藏/常用地址MCP工具类
 * 提供家、公司地址的设置和导航功能
 */
public class FavoriteTools {
    private static final String TAG = "FavoriteTools";
    
    private final BaseToolHelper baseHelper = BaseToolHelper.getInstance();

    /**
     * 设置家地址
     * @param latitude 纬度(必需)
     * @param longitude 经度(必需) 
     * @param poiName POI名称(可选)
     * @param address 地址描述(可选)
     * @return 设置结果
     */
    @McpTool(
        name = "set_home_address",
        title = "家地址设置工具", 
        description = "设置家的地址。调用示例：{\"latitude\":\"31.230390\",\"longitude\":\"121.473700\",\"poi_name\":\"上海迪士尼\",\"address\":\"上海市浦东新区\"}",
        returnDescription = "返回家地址设置结果",
        openWorldHint = true
    )
    public String setHomeAddress(
            @McpParam(name = "latitude",
                      description = "纬度。格式：数字字符串。示例：\"31.230390\"、\"39.904200\"。注意：必须是字符串格式的数字，带引号", 
                      required = true)
            String latitude,
            @McpParam(name = "longitude",
                      description = "经度。格式：数字字符串。示例：\"121.473700\"、\"116.407400\"。注意：必须是字符串格式的数字，带引号", 
                      required = true)
            String longitude,
            @McpParam(name = "poi_name",
                      description = "地点名称。格式：字符串类型。示例：\"上海迪士尼\"、\"北京天安门\"。可选参数", 
                      required = false)
            String poiName,
            @McpParam(name = "address",
                      description = "地址描述。格式：字符串类型。示例：\"上海市浦东新区川沙镇\"、\"北京市东城区\"。可选参数", 
                      required = false)
            String address) {
        
        try {
            // 检查必要参数
            if (latitude == null || latitude.trim().isEmpty() || 
                longitude == null || longitude.trim().isEmpty()) {
                Logger.e(TAG, "缺少必要的坐标参数 - latitude: " + latitude + ", longitude: " + longitude);
                JsonObject error = baseHelper.createErrorResponse("参数格式错误", "请提供有效的latitude和longitude参数");
                return Prompt.text(baseHelper.gson.toJson(error));
            }
            
            // 解析坐标参数
            double lat = Double.parseDouble(latitude);
            double lng = Double.parseDouble(longitude);
            
            Logger.i(TAG, "设置家地址: lat=" + lat + ", lon=" + lng + 
                    ", poiName=" + poiName + ", address=" + address);

                // 创建POI信息
                PoiInfoEntity poiInfo = new PoiInfoEntity();
                
                // 设置基本信息
                GeoPoint point = new GeoPoint(lng, lat);
                poiInfo.setPoint(point);
                
                // 设置POI ID（使用经纬度组合作为唯一标识）
                String pid = lng + "_" + lat;
                poiInfo.setPid(pid);
                
                // 设置名称，优先使用传入的名称，否则使用默认
                if (!TextUtils.isEmpty(poiName)) {
                    poiInfo.setName(poiName);
                } else if (!TextUtils.isEmpty(address)) {
                    poiInfo.setName(address);
                } else {
                    poiInfo.setName("家");
                }
                
                // 设置地址
                if (!TextUtils.isEmpty(address)) {
                    poiInfo.setAddress(address);
                } else if (!TextUtils.isEmpty(poiName)) {
                    poiInfo.setAddress(poiName);
                } else {
                    poiInfo.setAddress("家的位置");
                }
                
                // 创建收藏信息
                FavoriteInfo favoriteInfo = new FavoriteInfo();
                favoriteInfo.setCommonName(1); // 1表示家
                favoriteInfo.setItemId(pid);
                poiInfo.setFavoriteInfo(favoriteInfo);
                
                // 记录设置前的状态
                PoiInfoEntity oldHome = BehaviorPackage.getInstance().getHomeFavoriteInfo();
                Logger.i(TAG, "设置前的家地址: " + (oldHome != null ? 
                    oldHome.getName() + " (" + oldHome.getPoint().getLat() + "," + oldHome.getPoint().getLon() + ")" : "null"));
                
                // 先彻底清除所有旧的家地址数据，确保不会有多条记录
                Logger.i(TAG, "清除所有旧的家地址数据...");
                BehaviorPackage.getInstance().deleteFavoriteDataByType(1);
                Logger.i(TAG, "旧家地址数据清除完成");
                
                // 调用BehaviorPackage添加家地址
                Logger.i(TAG, "开始添加新的家地址: " + poiInfo.getName() + " (" + lat + "," + lng + ")");
                String result = BehaviorPackage.getInstance().addFavorite(poiInfo, 1);
                
                if (!TextUtils.isEmpty(result)) {
                    Logger.i(TAG, "家地址设置成功: " + result);
                    
                    // 验证设置结果
                    PoiInfoEntity newHome = BehaviorPackage.getInstance().getHomeFavoriteInfo();
                    Logger.i(TAG, "设置后验证家地址: " + (newHome != null ? 
                        newHome.getName() + " (" + newHome.getPoint().getLat() + "," + newHome.getPoint().getLon() + ")" : "null"));
                    
                    // 强制刷新TMC时间显示，确保基于新家地址坐标重新计算路线时间
                    try {
                        Logger.i(TAG, "开始刷新家地址TMC时间信息...");
                        
                        // 调用TMC刷新，基于新的家地址坐标重新计算路线时间
                        // 这将触发onRouteTMCInfo回调，最终更新UI的homeTime显示
                        RoutePackage.getInstance().refreshHomeOfficeTMC(MapType.MAIN_SCREEN_MAIN_MAP, true);
                        Logger.i(TAG, "已触发家地址TMC刷新，系统将重新计算基于新坐标的路线时间");
                        
                        // 等待TMC计算完成，避免立即验证时数据还未更新
                        Thread.sleep(500);
                        Logger.i(TAG, "TMC刷新等待完成");
                        
                        // BehaviorPackage.addFavorite()方法内部已经会调用notifyFavoriteStatusChanged()
                        // 不需要额外的反射调用，避免重复触发回调
                        
                    } catch (Exception e) {
                        Logger.w(TAG, "刷新家地址TMC时间信息失败: " + e.getMessage());
                    }
                    
                    // 创建成功响应
                    JsonObject successResponse = baseHelper.createSuccessResponse();
                    successResponse.addProperty("message", "家地址设置成功");
                    successResponse.addProperty("address", poiInfo.getAddress());
                    
                    JsonObject coordinates = new JsonObject();
                    coordinates.addProperty("latitude", lat);
                    coordinates.addProperty("longitude", lng);
                    successResponse.add("coordinates", coordinates);
                    
                    return Prompt.text(baseHelper.gson.toJson(successResponse));
                } else {
                    Logger.e(TAG, "家地址设置失败");
                    JsonObject error = baseHelper.createErrorResponse("家地址设置失败", "无法保存家地址信息");
                    return Prompt.text(baseHelper.gson.toJson(error));
                }
                
        } catch (NumberFormatException e) {
            Logger.e(TAG, "坐标数值格式错误", e);
            JsonObject error = baseHelper.createErrorResponse("坐标数值格式错误", "请提供有效的数字格式的经纬度坐标");
            return Prompt.text(baseHelper.gson.toJson(error));
        } catch (Exception e) {
            Logger.e(TAG, "设置家地址异常: " + e.getMessage());
            JsonObject error = baseHelper.createErrorResponse("设置家地址异常", e.getMessage());
            return Prompt.text(baseHelper.gson.toJson(error));
        }
    }

    /**
     * 一键回家 - 导航到家的地址
     * @return 导航结果
     */
    @McpTool(
        name = "go_home",
        title = "一键回家工具", 
        description = "当用户提及回家关键词时，一键导航回家。调用示例：{} (无需参数)",
        returnDescription = "返回导航到家的启动结果",
        openWorldHint = true
    )
    public String goHome() {
        try {
                Logger.i(TAG, "执行一键回家");
                
                // 1. 检查当前导航状态
                String currentNaviStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
                Logger.i(TAG, "当前导航状态: " + currentNaviStatus);
                
                // 2. 如果正在导航，先停止当前导航
                if (NaviStatus.NaviStatusType.NAVING.equals(currentNaviStatus) || 
                    NaviStatus.NaviStatusType.LIGHT_NAVING.equals(currentNaviStatus)) {
                    Logger.i(TAG, "检测到正在导航，先停止当前导航");
                    
                    boolean stopResult = NaviPackage.getInstance().stopNavigation(true);
                    Logger.i(TAG, "停止导航结果: " + stopResult);
                    
                    if (!stopResult) {
                        Logger.w(TAG, "停止当前导航失败，但继续执行回家导航");
                    }
                    
                    // 等待导航状态稳定
                    try {
                        Thread.sleep(800);
                        Logger.i(TAG, "等待导航状态稳定完成");
                    } catch (InterruptedException e) {
                        Logger.w(TAG, "等待导航状态稳定被中断: " + e.getMessage());
                        Thread.currentThread().interrupt();
                    }
                }
                
                // 3. 获取家的地址信息
                PoiInfoEntity homeInfo = BehaviorPackage.getInstance().getHomeFavoriteInfo();
                
                if (homeInfo == null || homeInfo.getPoint() == null) {
                    Logger.w(TAG, "未设置家地址");
                    
                    JsonObject error = baseHelper.createErrorResponse("未设置家地址", "请先使用set_home_address设置家的地址");
                    return Prompt.text(baseHelper.gson.toJson(error));
                }
                
                // 构建路线请求参数
                RouteSpeechRequestParam requestParam = new RouteSpeechRequestParam();
                requestParam.setMEndPoiInfoEntity(homeInfo);
                requestParam.setMMapTypeId(MapType.MAIN_SCREEN_MAIN_MAP);
                
                // 使用语音界面流程，让界面内部处理路线规划
                Logger.i(TAG, "开始回家导航: " + homeInfo.getName() + 
                        ", 地址: " + homeInfo.getAddress());
                
                // 创建Bundle，使用标准语音流程
                Bundle bundle = new Bundle();
                bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, 
                             IVrBridgeConstant.VoiceIntentPage.ROUTING);
                             
                // 使用正确的语音流程参数key
                bundle.putParcelable("speech_open_route", requestParam);
                
                Logger.i(TAG, "调用语音界面流程，让系统内部处理路线规划和导航启动");
                MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
                
                // 创建成功响应
                JsonObject successResponse = baseHelper.createSuccessResponse();
                
                // 根据之前是否在导航中来设置消息
                String message = NaviStatus.NaviStatusType.NAVING.equals(currentNaviStatus) || 
                                NaviStatus.NaviStatusType.LIGHT_NAVING.equals(currentNaviStatus) ?
                                "已停止当前导航，开始导航到家" : "开始导航到家";
                                
                successResponse.addProperty("message", message);
                successResponse.addProperty("previous_navigation_stopped", 
                    NaviStatus.NaviStatusType.NAVING.equals(currentNaviStatus) || 
                    NaviStatus.NaviStatusType.LIGHT_NAVING.equals(currentNaviStatus));
                successResponse.addProperty("previous_navigation_status", currentNaviStatus);
                
                JsonObject destination = new JsonObject();
                destination.addProperty("name", homeInfo.getName());
                destination.addProperty("address", homeInfo.getAddress());
                
                JsonObject coordinates = new JsonObject();
                coordinates.addProperty("latitude", homeInfo.getPoint().getLat());
                coordinates.addProperty("longitude", homeInfo.getPoint().getLon());
                destination.add("coordinates", coordinates);
                
                successResponse.add("destination", destination);
                
                String result = baseHelper.gson.toJson(successResponse);
                Logger.i(TAG, "回家导航结果: " + result);
                
                return result;
                
        } catch (SecurityException e) {
            Logger.e(TAG, "导航权限被拒绝: " + e.getMessage());
            JsonObject error = baseHelper.createErrorResponse("导航权限被拒绝", "应用缺少导航相关权限：" + e.getMessage());
            return Prompt.text(baseHelper.gson.toJson(error));
        } catch (IllegalStateException e) {
            Logger.e(TAG, "导航状态异常: " + e.getMessage());
            JsonObject error = baseHelper.createErrorResponse("导航状态异常", "系统导航状态不正确：" + e.getMessage());
            return Prompt.text(baseHelper.gson.toJson(error));
        } catch (Exception e) {
            Logger.e(TAG, "一键回家失败: " + e.getMessage(), e);
            JsonObject error = baseHelper.createErrorResponse("导航失败", "系统内部错误：" + e.getMessage());
            error.addProperty("error_type", e.getClass().getSimpleName());
            return Prompt.text(baseHelper.gson.toJson(error));
        }
    }

    /**
     * 设置公司地址
     * @param latitude 纬度(必需)
     * @param longitude 经度(必需) 
     * @param poiName POI名称(可选)
     * @param address 地址描述(可选)
     * @return 设置结果
     */
    @McpTool(
        name = "set_company_address",
        title = "公司地址设置工具", 
        description = "设置公司的地址。调用示例：{\"latitude\":\"31.230390\",\"longitude\":\"121.473700\",\"poi_name\":\"陆家嘴金融中心\",\"address\":\"上海市浦东新区陆家嘴\"}",
        returnDescription = "返回公司地址设置结果",
        openWorldHint = true
    )
    public String setCompanyAddress(
            @McpParam(name = "latitude",
                      description = "纬度。格式：数字字符串。示例：\"31.230390\"、\"39.904200\"。注意：必须是字符串格式的数字，带引号", 
                      required = true)
            String latitude,
            @McpParam(name = "longitude",
                      description = "经度。格式：数字字符串。示例：\"121.473700\"、\"116.407400\"。注意：必须是字符串格式的数字，带引号", 
                      required = true)
            String longitude,
            @McpParam(name = "poi_name",
                      description = "地点名称。格式：字符串类型。示例：\"陆家嘴金融中心\"、\"中关村软件园\"。可选参数", 
                      required = false)
            String poiName,
            @McpParam(name = "address",
                      description = "地址描述。格式：字符串类型。示例：\"上海市浦东新区陆家嘴\"、\"北京市海淀区中关村\"。可选参数", 
                      required = false)
            String address) {
        
        try {
            // 检查必要参数
            if (latitude == null || latitude.trim().isEmpty() || 
                longitude == null || longitude.trim().isEmpty()) {
                Logger.e(TAG, "缺少必要的坐标参数 - latitude: " + latitude + ", longitude: " + longitude);
                JsonObject error = baseHelper.createErrorResponse("参数格式错误", "请提供有效的latitude和longitude参数");
                return Prompt.text(baseHelper.gson.toJson(error));
            }
            
            // 解析坐标参数
            double lat = Double.parseDouble(latitude);
            double lng = Double.parseDouble(longitude);
            
            Logger.i(TAG, "设置公司地址: lat=" + lat + ", lon=" + lng + 
                    ", poiName=" + poiName + ", address=" + address);

                // 创建POI信息
                PoiInfoEntity poiInfo = new PoiInfoEntity();
                
                // 设置基本信息
                GeoPoint point = new GeoPoint(lng, lat);
                poiInfo.setPoint(point);
                
                // 设置POI ID（使用经纬度组合作为唯一标识）
                String pid = lng + "_" + lat;
                poiInfo.setPid(pid);
                
                // 设置名称，优先使用传入的名称，否则使用默认
                if (!TextUtils.isEmpty(poiName)) {
                    poiInfo.setName(poiName);
                } else if (!TextUtils.isEmpty(address)) {
                    poiInfo.setName(address);
                } else {
                    poiInfo.setName("公司");
                }
                
                // 设置地址
                if (!TextUtils.isEmpty(address)) {
                    poiInfo.setAddress(address);
                } else if (!TextUtils.isEmpty(poiName)) {
                    poiInfo.setAddress(poiName);
                } else {
                    poiInfo.setAddress("公司的位置");
                }
                
                // 创建收藏信息
                FavoriteInfo favoriteInfo = new FavoriteInfo();
                favoriteInfo.setCommonName(2); // 2表示公司
                favoriteInfo.setItemId(pid);
                poiInfo.setFavoriteInfo(favoriteInfo);
                
                // 记录设置前的状态
                PoiInfoEntity oldCompany = BehaviorPackage.getInstance().getCompanyFavoriteInfo();
                Logger.i(TAG, "设置前的公司地址: " + (oldCompany != null ? 
                    oldCompany.getName() + " (" + oldCompany.getPoint().getLat() + "," + oldCompany.getPoint().getLon() + ")" : "null"));
                
                // 先彻底清除所有旧的公司地址数据，确保不会有多条记录
                Logger.i(TAG, "清除所有旧的公司地址数据...");
                BehaviorPackage.getInstance().deleteFavoriteDataByType(2);
                Logger.i(TAG, "旧公司地址数据清除完成");
                
                // 调用BehaviorPackage添加公司地址
                Logger.i(TAG, "开始添加新的公司地址: " + poiInfo.getName() + " (" + lat + "," + lng + ")");
                String result = BehaviorPackage.getInstance().addFavorite(poiInfo, 2);
                
                if (!TextUtils.isEmpty(result)) {
                    Logger.i(TAG, "公司地址设置成功: " + result);
                    
                    // 验证设置结果
                    PoiInfoEntity newCompany = BehaviorPackage.getInstance().getCompanyFavoriteInfo();
                    Logger.i(TAG, "设置后验证公司地址: " + (newCompany != null ? 
                        newCompany.getName() + " (" + newCompany.getPoint().getLat() + "," + newCompany.getPoint().getLon() + ")" : "null"));
                    
                    // 强制刷新TMC时间显示，确保基于新公司地址坐标重新计算路线时间
                    try {
                        Logger.i(TAG, "开始刷新公司地址TMC时间信息...");
                        
                        // 调用TMC刷新，基于新的公司地址坐标重新计算路线时间
                        // 这将触发onRouteTMCInfo回调，最终更新UI的companyTime显示
                        RoutePackage.getInstance().refreshHomeOfficeTMC(MapType.MAIN_SCREEN_MAIN_MAP, false);
                        Logger.i(TAG, "已触发公司地址TMC刷新，系统将重新计算基于新坐标的路线时间");
                        
                        // 等待TMC计算完成，避免立即验证时数据还未更新
                        Thread.sleep(500);
                        Logger.i(TAG, "TMC刷新等待完成");
                        
                        // BehaviorPackage.addFavorite()方法内部已经会调用notifyFavoriteStatusChanged()
                        // 不需要额外的反射调用，避免重复触发回调
                        
                    } catch (Exception e) {
                        Logger.w(TAG, "刷新公司地址TMC时间信息失败: " + e.getMessage());
                    }
                    
                    // 创建成功响应
                    JsonObject successResponse = baseHelper.createSuccessResponse();
                    successResponse.addProperty("message", "公司地址设置成功");
                    successResponse.addProperty("address", poiInfo.getAddress());
                    
                    JsonObject coordinates = new JsonObject();
                    coordinates.addProperty("latitude", lat);
                    coordinates.addProperty("longitude", lng);
                    successResponse.add("coordinates", coordinates);
                    
                    return Prompt.text(baseHelper.gson.toJson(successResponse));
                } else {
                    Logger.e(TAG, "公司地址设置失败");
                    JsonObject error = baseHelper.createErrorResponse("公司地址设置失败", "无法保存公司地址信息");
                    return Prompt.text(baseHelper.gson.toJson(error));
                }
                
        } catch (NumberFormatException e) {
            Logger.e(TAG, "坐标数值格式错误", e);
            JsonObject error = baseHelper.createErrorResponse("坐标数值格式错误", "请提供有效的数字格式的经纬度坐标");
            return Prompt.text(baseHelper.gson.toJson(error));
        } catch (Exception e) {
            Logger.e(TAG, "设置公司地址异常: " + e.getMessage());
            JsonObject error = baseHelper.createErrorResponse("设置公司地址异常", e.getMessage());
            return Prompt.text(baseHelper.gson.toJson(error));
        }
    }

    /**
     * 一键去公司 - 导航到公司的地址
     * @return 导航结果
     */
    @McpTool(
        name = "go_to_company",
        title = "一键去公司工具", 
        description = "当用户提及去公司关键词时，一键导航去公司。调用示例：{} (无需参数)",
        returnDescription = "返回导航到公司的启动结果",
        openWorldHint = true
    )
    public String goToCompany() {
        try {
                Logger.i(TAG, "执行一键去公司");
                
                // 1. 检查当前导航状态
                String currentNaviStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
                Logger.i(TAG, "当前导航状态: " + currentNaviStatus);
                
                // 2. 如果正在导航，先停止当前导航
                if (NaviStatus.NaviStatusType.NAVING.equals(currentNaviStatus) || 
                    NaviStatus.NaviStatusType.LIGHT_NAVING.equals(currentNaviStatus)) {
                    Logger.i(TAG, "检测到正在导航，先停止当前导航");
                    
                    boolean stopResult = NaviPackage.getInstance().stopNavigation(true);
                    Logger.i(TAG, "停止导航结果: " + stopResult);
                    
                    if (!stopResult) {
                        Logger.w(TAG, "停止当前导航失败，但继续执行去公司导航");
                    }
                    
                    // 等待导航状态稳定
                    try {
                        Thread.sleep(800);
                        Logger.i(TAG, "等待导航状态稳定完成");
                    } catch (InterruptedException e) {
                        Logger.w(TAG, "等待导航状态稳定被中断: " + e.getMessage());
                        Thread.currentThread().interrupt();
                    }
                }
                
                // 3. 获取公司的地址信息
                PoiInfoEntity companyInfo = BehaviorPackage.getInstance().getCompanyFavoriteInfo();
                
                if (companyInfo == null || companyInfo.getPoint() == null) {
                    Logger.w(TAG, "未设置公司地址");
                    
                    JsonObject error = baseHelper.createErrorResponse("未设置公司地址", "请先使用set_company_address设置公司的地址");
                    return Prompt.text(baseHelper.gson.toJson(error));
                }
                
                // 构建路线请求参数
                RouteSpeechRequestParam requestParam = new RouteSpeechRequestParam();
                requestParam.setMEndPoiInfoEntity(companyInfo);
                requestParam.setMMapTypeId(MapType.MAIN_SCREEN_MAIN_MAP);
                
                // 使用语音界面流程，让界面内部处理路线规划
                Logger.i(TAG, "开始去公司导航: " + companyInfo.getName() + 
                        ", 地址: " + companyInfo.getAddress());
                
                // 创建Bundle，使用标准语音流程
                Bundle bundle = new Bundle();
                bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, 
                             IVrBridgeConstant.VoiceIntentPage.ROUTING);
                             
                // 使用正确的语音流程参数key
                bundle.putParcelable("speech_open_route", requestParam);
                
                Logger.i(TAG, "调用语音界面流程，让系统内部处理路线规划和导航启动");
                MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
                
                // 创建成功响应
                JsonObject successResponse = baseHelper.createSuccessResponse();
                
                // 根据之前是否在导航中来设置消息
                String message = NaviStatus.NaviStatusType.NAVING.equals(currentNaviStatus) || 
                                NaviStatus.NaviStatusType.LIGHT_NAVING.equals(currentNaviStatus) ?
                                "已停止当前导航，开始导航到公司" : "开始导航到公司";
                                
                successResponse.addProperty("message", message);
                successResponse.addProperty("previous_navigation_stopped", 
                    NaviStatus.NaviStatusType.NAVING.equals(currentNaviStatus) || 
                    NaviStatus.NaviStatusType.LIGHT_NAVING.equals(currentNaviStatus));
                successResponse.addProperty("previous_navigation_status", currentNaviStatus);
                
                JsonObject destination = new JsonObject();
                destination.addProperty("name", companyInfo.getName());
                destination.addProperty("address", companyInfo.getAddress());
                
                JsonObject coordinates = new JsonObject();
                coordinates.addProperty("latitude", companyInfo.getPoint().getLat());
                coordinates.addProperty("longitude", companyInfo.getPoint().getLon());
                destination.add("coordinates", coordinates);
                
                successResponse.add("destination", destination);
                
                String result = baseHelper.gson.toJson(successResponse);
                Logger.i(TAG, "去公司导航结果: " + result);
                
                return result;
                
        } catch (SecurityException e) {
            Logger.e(TAG, "导航权限被拒绝: " + e.getMessage());
            JsonObject error = baseHelper.createErrorResponse("导航权限被拒绝", "应用缺少导航相关权限：" + e.getMessage());
            return Prompt.text(baseHelper.gson.toJson(error));
        } catch (IllegalStateException e) {
            Logger.e(TAG, "导航状态异常: " + e.getMessage());
            JsonObject error = baseHelper.createErrorResponse("导航状态异常", "系统导航状态不正确：" + e.getMessage());
            return Prompt.text(baseHelper.gson.toJson(error));
        } catch (Exception e) {
            Logger.e(TAG, "一键去公司失败: " + e.getMessage(), e);
            JsonObject error = baseHelper.createErrorResponse("导航失败", "系统内部错误：" + e.getMessage());
            error.addProperty("error_type", e.getClass().getSimpleName());
            return Prompt.text(baseHelper.gson.toJson(error));
        }
    }
}
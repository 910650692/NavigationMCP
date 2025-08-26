package com.sgm.navi.mcp.tools;

import android.util.Log;
import com.google.gson.JsonObject;
// 原始MCP注解
import com.sgm.navi.mcp.core.MCPDataType;
import com.sgm.navi.mcp.core.MCPTool;
import com.sgm.navi.service.define.position.LocInfoBean;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.logicpaket.search.SearchPackage;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * 位置相关MCP工具类
 * 负责处理车辆定位、地址解析等位置相关功能
 */
public class LocationTools {
    private static final String TAG = "LocationTools";
    private final BaseToolHelper baseHelper = BaseToolHelper.getInstance();

    /**
     * 获取当前位置
     */
    @MCPTool(
        name = "get_current_location",
        title = "当前位置获取工具",
        description = "获取车辆当前完整位置信息，包括GPS坐标(经纬度)、定位精度、详细地址、行驶速度和方向等。",
        returnDescription = "返回JSON格式的位置信息，包含经纬度、精度、地址等字段",
        returnType = MCPDataType.STRING,
        readOnlyHint = true
    )
    public String getCurrentLocation() {
        Log.d(TAG, "获取当前位置");
        
        try {
            // 调用真实的SGM定位服务
            LocInfoBean locInfo = baseHelper.getCurrentPosition();
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
                
                String result = baseHelper.gson.toJson(locationInfo);
                Log.d(TAG, "真实位置: " + result);
                return result;
            } else {
                // Fallback: 如果真实API返回null，提供错误信息
                JsonObject error = baseHelper.createErrorResponse("无法获取位置信息", "定位服务可能未就绪");
                
                String result = baseHelper.gson.toJson(error);
                Log.w(TAG, "位置获取失败: " + result);
                return result;
            }
            
        } catch (Exception e) {
            Log.e(TAG, "获取位置失败", e);
            JsonObject error = baseHelper.createErrorResponse("获取位置失败", e.getMessage());
            return baseHelper.gson.toJson(error);
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
            // 验证坐标有效性
            if (Math.abs(latitude) > 90 || Math.abs(longitude) > 180) {
                Log.e(TAG, "坐标超出有效范围");
                return null;
            }
            
            // 创建CompletableFuture
            CompletableFuture<String> future = new CompletableFuture<>();
            
            // 设置5秒超时
            final int timeoutSeconds = 5;
            future = future.completeOnTimeout(null, timeoutSeconds * 1000, TimeUnit.MILLISECONDS);
            
            // 创建GeoPoint
            GeoPoint geoPoint = new GeoPoint();
            geoPoint.setLat(latitude);
            geoPoint.setLon(longitude);
            
            // 发起静默逆地理搜索，获取taskId
            int taskId = SearchPackage.getInstance().geoSearch(geoPoint, true);
            
            if (taskId <= 0) {
                Log.e(TAG, "搜索服务未就绪");
                return null;
            }
            
            // 将taskId和future关联
            baseHelper.addGeoSearchTask(taskId, future);
            
            // 等待结果
            String result = future.get(timeoutSeconds * 1000, TimeUnit.MILLISECONDS);
            
            // 清理任务
            baseHelper.removeGeoSearchTask(taskId);
            
            return result;
            
        } catch (TimeoutException e) {
            Log.w(TAG, "逆地理搜索超时");
            return null;
        } catch (Exception e) {
            Log.w(TAG, "逆地理搜索失败: " + e.getMessage());
            return null;
        }
    }
}
package com.sgm.navi.mcp.tools;

import android.util.Log;
import com.google.gson.JsonObject;
// 新MCP SDK注解
import com.sgm.mcp.protocol.annotations.McpTool;
import com.sgm.mcp.protocol.annotations.McpParam;
import com.sgm.mcp.protocol.McpSpec;
import com.sgm.mcp.protocol.Prompt;
import com.sgm.navi.service.logicpaket.setting.SettingPackage;
import com.sgm.navi.service.define.route.RoutePreferenceID;

/**
 * 导航设置相关MCP工具类
 * 提供导航相关设置的查询和修改功能，包括车牌号码、限行设置、路线偏好等
 */
public class SettingTools {
    private static final String TAG = "SettingTools";
    private final BaseToolHelper baseHelper = BaseToolHelper.getInstance();

    /**
     * 设置车牌号码
     */
    @McpTool(
        name = "set_plate_number",
        title = "车牌号码设置工具",
        description = "设置车牌号码，用于限行判断。格式示例：沪A12345、京B88888。设置车牌后会自动开启避开限行功能。",
        returnDescription = "返回车牌号码设置结果",
        promptMessageContent = McpSpec.PromptMessageContent.Text,
        openWorldHint = true
    )
    public String setPlateNumber(
            @McpParam(name = "plate_number",
                      description = "车牌号码，如：沪A12345、京B88888")
            String plateNumber) {
        
        Log.d(TAG, "设置车牌号码: " + plateNumber);
        
        if (plateNumber == null || plateNumber.trim().isEmpty()) {
            JsonObject error = baseHelper.createErrorResponse("参数错误", "车牌号码不能为空");
            return Prompt.text(baseHelper.gson.toJson(error));
        }
        
        // 简单的车牌格式验证
        String trimmedPlate = plateNumber.trim();
        if (trimmedPlate.length() < 7 || trimmedPlate.length() > 8) {
            JsonObject error = baseHelper.createErrorResponse("格式错误", "请输入有效的车牌号码，如：沪A12345");
            return Prompt.text(baseHelper.gson.toJson(error));
        }
        
        try {
            SettingPackage.getInstance().setConfigKeyPlateNumber(trimmedPlate);
            
            // 设置车牌后自动开启避开限行
            SettingPackage.getInstance().setConfigKeyAvoidLimit(true);
            
            JsonObject result = baseHelper.createSuccessResponse();
            result.addProperty("message", "车牌设置成功，已自动开启避开限行功能");
            result.addProperty("plate_number", trimmedPlate);
            result.addProperty("avoid_limit_enabled", true);
            
            Log.d(TAG, "车牌设置成功: " + trimmedPlate);
            return Prompt.text(baseHelper.gson.toJson(result));
            
        } catch (Exception e) {
            Log.e(TAG, "设置车牌失败", e);
            JsonObject error = baseHelper.createErrorResponse("设置失败", e.getMessage());
            return Prompt.text(baseHelper.gson.toJson(error));
        }
    }

    /**
     * 获取当前车牌号码
     */
    @McpTool(
        name = "get_plate_number",
        title = "车牌号码查询工具",
        description = "获取当前设置的车牌号码和限行功能状态",
        returnDescription = "返回当前车牌号码和限行设置信息",
        promptMessageContent = McpSpec.PromptMessageContent.Text,
        readOnlyHint = true
    )
    public String getPlateNumber() {
        try {
            String plateNumber = SettingPackage.getInstance().getConfigKeyPlateNumber();
            boolean avoidLimit = SettingPackage.getInstance().getConfigKeyAvoidLimit();
            
            JsonObject result = baseHelper.createSuccessResponse();
            result.addProperty("plate_number", plateNumber != null ? plateNumber : "");
            result.addProperty("has_plate", plateNumber != null && !plateNumber.trim().isEmpty());
            result.addProperty("avoid_limit_enabled", avoidLimit);
            
            if (plateNumber == null || plateNumber.trim().isEmpty()) {
                result.addProperty("message", "未设置车牌号码");
            } else {
                result.addProperty("message", "当前车牌: " + plateNumber + ", 避开限行: " + (avoidLimit ? "已开启" : "已关闭"));
            }
            
            return Prompt.text(baseHelper.gson.toJson(result));
            
        } catch (Exception e) {
            Log.e(TAG, "获取车牌失败", e);
            JsonObject error = baseHelper.createErrorResponse("获取失败", e.getMessage());
            return Prompt.text(baseHelper.gson.toJson(error));
        }
    }

    /**
     * 设置是否避开限行
     */
    @McpTool(
        name = "set_avoid_limit",
        title = "限行设置工具",
        description = "设置是否避开限行。注意：需要先设置车牌号码才能使用限行功能。",
        returnDescription = "返回限行设置结果",
        promptMessageContent = McpSpec.PromptMessageContent.Text,
        openWorldHint = true
    )
    public String setAvoidLimit(
            @McpParam(name = "avoid_limit",
                      description = "true - 开启避开限行，false - 关闭避开限行")
            boolean avoidLimit) {
        
        Log.d(TAG, "设置避开限行: " + avoidLimit);
        
        try {
            // 如果要开启限行，检查车牌是否已设置
            if (avoidLimit) {
                String plateNumber = SettingPackage.getInstance().getConfigKeyPlateNumber();
                if (plateNumber == null || plateNumber.trim().isEmpty()) {
                    JsonObject error = baseHelper.createErrorResponse(
                        "限行功能需要车牌", 
                        "请先设置车牌号码才能开启避开限行功能"
                    );
                    error.addProperty("suggestion", "请先调用set_plate_number设置车牌");
                    return Prompt.text(baseHelper.gson.toJson(error));
                }
            }
            
            SettingPackage.getInstance().setConfigKeyAvoidLimit(avoidLimit);
            
            JsonObject result = baseHelper.createSuccessResponse();
            result.addProperty("message", "避开限行设置" + (avoidLimit ? "已开启" : "已关闭"));
            result.addProperty("avoid_limit_enabled", avoidLimit);
            
            Log.d(TAG, "避开限行设置成功: " + avoidLimit);
            return Prompt.text(baseHelper.gson.toJson(result));
            
        } catch (Exception e) {
            Log.e(TAG, "设置避开限行失败", e);
            JsonObject error = baseHelper.createErrorResponse("设置失败", e.getMessage());
            return Prompt.text(baseHelper.gson.toJson(error));
        }
    }

    /**
     * 获取是否避开限行设置
     */
    @McpTool(
        name = "get_avoid_limit",
        title = "限行设置查询工具",
        description = "获取当前的避开限行设置状态",
        returnDescription = "返回当前限行设置状态",
        promptMessageContent = McpSpec.PromptMessageContent.Text,
        readOnlyHint = true
    )
    public String getAvoidLimit() {
        try {
            boolean avoidLimit = SettingPackage.getInstance().getConfigKeyAvoidLimit();
            String plateNumber = SettingPackage.getInstance().getConfigKeyPlateNumber();
            boolean hasPlate = plateNumber != null && !plateNumber.trim().isEmpty();
            
            JsonObject result = baseHelper.createSuccessResponse();
            result.addProperty("avoid_limit_enabled", avoidLimit);
            result.addProperty("has_plate", hasPlate);
            result.addProperty("plate_number", hasPlate ? plateNumber : "");
            
            String status = avoidLimit ? "已开启" : "已关闭";
            if (avoidLimit && !hasPlate) {
                result.addProperty("message", "避开限行" + status + "，但未设置车牌号码，功能可能无效");
                result.addProperty("warning", "请设置车牌号码以确保限行功能正常工作");
            } else {
                result.addProperty("message", "避开限行" + status);
            }
            
            return Prompt.text(baseHelper.gson.toJson(result));
            
        } catch (Exception e) {
            Log.e(TAG, "获取避开限行设置失败", e);
            JsonObject error = baseHelper.createErrorResponse("获取失败", e.getMessage());
            return Prompt.text(baseHelper.gson.toJson(error));
        }
    }

    /**
     * 获取所有导航相关设置的综合信息
     */
    @McpTool(
        name = "get_navigation_settings",
        title = "导航设置综合查询工具",
        description = "获取所有导航相关设置的综合信息，包括车牌、限行、路线偏好等",
        returnDescription = "返回完整的导航设置信息",
        promptMessageContent = McpSpec.PromptMessageContent.Text,
        readOnlyHint = true
    )
    public String getNavigationSettings() {
        try {
            String plateNumber = SettingPackage.getInstance().getConfigKeyPlateNumber();
            boolean avoidLimit = SettingPackage.getInstance().getConfigKeyAvoidLimit();
            RoutePreferenceID routePreference = SettingPackage.getInstance().getRoutePreference();
            boolean hasPlate = plateNumber != null && !plateNumber.trim().isEmpty();
            
            JsonObject result = baseHelper.createSuccessResponse();
            result.addProperty("plate_number", hasPlate ? plateNumber : "");
            result.addProperty("has_plate", hasPlate);
            result.addProperty("avoid_limit_enabled", avoidLimit);
            result.addProperty("route_preference", routePreference != null ? routePreference.name() : "PREFERENCE_RECOMMEND");
            
            // 添加友好的状态描述
            StringBuilder statusBuilder = new StringBuilder();
            statusBuilder.append("车牌: ").append(hasPlate ? plateNumber : "未设置").append(", ");
            statusBuilder.append("避开限行: ").append(avoidLimit ? "已开启" : "已关闭").append(", ");
            statusBuilder.append("路线偏好: ").append(getRoutePreferenceDescription(routePreference));
            
            result.addProperty("message", statusBuilder.toString());
            
            // 添加警告信息
            if (avoidLimit && !hasPlate) {
                result.addProperty("warning", "已开启避开限行但未设置车牌，限行功能可能无效");
            }
            
            return Prompt.text(baseHelper.gson.toJson(result));
            
        } catch (Exception e) {
            Log.e(TAG, "获取导航设置失败", e);
            JsonObject error = baseHelper.createErrorResponse("获取失败", e.getMessage());
            return Prompt.text(baseHelper.gson.toJson(error));
        }
    }

    /**
     * 获取路线偏好的中文描述
     */
    private String getRoutePreferenceDescription(RoutePreferenceID preference) {
        if (preference == null) return "推荐路线";
        
        switch (preference) {
            case PREFERENCE_RECOMMEND: return "推荐路线";
            case PREFERENCE_AVOIDCONGESTION: return "躲避拥堵";
            case PREFERENCE_LESSCHARGE: return "少收费";
            case PREFERENCE_NOTHIGHWAY: return "不走高速";
            case PREFERENCE_FIRSTHIGHWAY: return "高速优先";
            case PREFERENCE_FASTESTSPEED: return "时间最短";
            default: return "推荐路线";
        }
    }
}
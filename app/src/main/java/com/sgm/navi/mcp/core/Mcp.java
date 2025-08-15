package com.sgm.navi.mcp.core;

import android.os.RemoteException;
import android.util.Log;

import com.sgm.navi.mcp.IMCPService;

import java.util.HashMap;
import java.util.Map;

/**
 * MCP核心引擎
 * 提供注解驱动的工具注册和调用功能
 */
public class Mcp {
    private static final String TAG = "Mcp";
    
    // MCP协调中心服务连接
    private static IMCPService mcpCoordinatorService;
    
    // 已注册的工具实例映射
    private static final Map<String, Object> registeredToolInstances = new HashMap<>();
    
    // 已注册的工具信息映射
    private static final Map<String, AnnotationScanner.ToolInfo> registeredToolInfos = new HashMap<>();
    
    /**
     * 设置MCP协调中心服务连接
     * @param service MCP协调中心服务
     */
    public static void setMCPCoordinatorService(IMCPService service) {
        mcpCoordinatorService = service;
        Log.d(TAG, "MCP协调中心服务连接已设置");
    }
    
    /**
     * 注解驱动的工具注册
     * 扫描工具实例中的@MCPTool注解方法并自动注册
     * @param toolInstance 工具实例对象
     */
    public static void registerTool(Object toolInstance) {
        if (mcpCoordinatorService == null) {
            Log.e(TAG, "MCP协调中心服务未设置，无法注册工具");
            return;
        }
        
        try {
            // 扫描工具注解
            Map<String, AnnotationScanner.ToolInfo> tools = AnnotationScanner.scanForMCPTools(toolInstance);
            
            // 注册每个工具
            for (Map.Entry<String, AnnotationScanner.ToolInfo> entry : tools.entrySet()) {
                String toolName = entry.getKey();
                AnnotationScanner.ToolInfo toolInfo = entry.getValue();
                
                // 生成Schema
                String schema = SchemaGenerator.generateSchema(toolInfo);
                
                // 注册到MCP协调中心
                mcpCoordinatorService.registerTool(
                    toolName, 
                    toolInfo.description, 
                    schema, 
                    "com.sgm.navi.hmi"
                );
                
                // 保存工具实例和信息
                registeredToolInstances.put(toolName, toolInstance);
                registeredToolInfos.put(toolName, toolInfo);
                
                Log.d(TAG, "✅ 工具注册成功: " + toolName);
            }
            
            Log.d(TAG, "🎉 注解驱动注册完成，共注册 " + tools.size() + " 个工具");
            
        } catch (RemoteException e) {
            Log.e(TAG, "注册工具失败: " + e.getMessage(), e);
        }
    }
    
    /**
     * 调用已注册的工具
     * @param toolName 工具名称
     * @param parameters 参数JSON字符串
     * @return 工具执行结果
     */
    public static String invokeTool(String toolName, String parameters) {
        Object toolInstance = registeredToolInstances.get(toolName);
        AnnotationScanner.ToolInfo toolInfo = registeredToolInfos.get(toolName);
        
        if (toolInstance == null || toolInfo == null) {
            Log.e(TAG, "工具未找到: " + toolName);
            return "{\"error\":\"工具未找到: " + toolName + "\"}";
        }
        
        Log.d(TAG, "🔧 调用工具: " + toolName + ", 参数: " + parameters);
        
        String result = ReflectionInvoker.invokeTool(toolInstance, toolInfo, parameters);
        
        Log.d(TAG, "✅ 工具执行完成: " + toolName + ", 结果: " + result);
        
        return result;
    }
    
    /**
     * 获取已注册工具数量
     */
    public static int getRegisteredToolCount() {
        return registeredToolInstances.size();
    }
    
    /**
     * 获取已注册工具名称数组
     */
    public static String[] getRegisteredToolNames() {
        return registeredToolInstances.keySet().toArray(new String[0]);
    }
    
    /**
     * 检查工具是否已注册
     */
    public static boolean isToolRegistered(String toolName) {
        return registeredToolInstances.containsKey(toolName);
    }
}
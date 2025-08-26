package com.sgm.navi.mcp.coordinator;

import android.util.Log;

import java.util.HashMap;
import java.util.Map;

/**
 * MCP工具注册中心
 * 负责管理所有注册的MCP工具
 */
public class ToolRegistry {
    private static final String TAG = "ToolRegistry";
    
    private final Map<String, RegisteredTool> registeredTools = new HashMap<>();
    
    /**
     * 注册工具
     */
    public synchronized void registerTool(String name, String description, String schema, String packageName) {
        RegisteredTool tool = new RegisteredTool(name, description, schema, packageName);
        registeredTools.put(name, tool);
        Log.d(TAG, "工具注册成功: " + name + " from " + packageName);
    }
    
    /**
     * 获取工具信息
     */
    public synchronized RegisteredTool getTool(String name) {
        return registeredTools.get(name);
    }
    
    /**
     * 获取所有工具名称
     */
    public synchronized String[] getAllToolNames() {
        return registeredTools.keySet().toArray(new String[0]);
    }
    
    /**
     * 注销工具
     */
    public synchronized void unregisterTool(String name) {
        RegisteredTool removed = registeredTools.remove(name);
        if (removed != null) {
            Log.d(TAG, "工具注销成功: " + name + " from " + removed.packageName);
        } else {
            Log.w(TAG, "尝试注销不存在的工具: " + name);
        }
    }
    
    /**
     * 工具是否存在
     */
    public synchronized boolean hasTool(String name) {
        return registeredTools.containsKey(name);
    }
    
    /**
     * 获取工具数量
     */
    public synchronized int getToolCount() {
        return registeredTools.size();
    }
    
    /**
     * 已注册工具信息
     */
    public static class RegisteredTool {
        public final String name;
        public final String description;
        public final String schema;
        public final String packageName;
        
        public RegisteredTool(String name, String description, String schema, String packageName) {
            this.name = name;
            this.description = description;
            this.schema = schema;
            this.packageName = packageName;
        }
    }
}
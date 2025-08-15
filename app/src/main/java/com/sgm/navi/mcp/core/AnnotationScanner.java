package com.sgm.navi.mcp.core;

import android.util.Log;

import java.lang.reflect.Method;
import java.lang.reflect.Parameter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * MCP工具注解扫描器
 * 负责扫描类中的@MCPTool注解方法，提取工具信息
 */
public class AnnotationScanner {
    private static final String TAG = "AnnotationScanner";

    /**
     * 扫描对象中的MCP工具
     * @param toolInstance 工具实例对象
     * @return 工具信息映射 (toolName -> ToolInfo)
     */
    public static Map<String, ToolInfo> scanForMCPTools(Object toolInstance) {
        Map<String, ToolInfo> tools = new HashMap<>();
        
        Class<?> clazz = toolInstance.getClass();
        Method[] methods = clazz.getDeclaredMethods();
        
        for (Method method : methods) {
            MCPTool toolAnnotation = method.getAnnotation(MCPTool.class);
            if (toolAnnotation != null) {
                ToolInfo toolInfo = createToolInfo(method, toolAnnotation);
                tools.put(toolAnnotation.name(), toolInfo);
                Log.d(TAG, "发现MCP工具: " + toolAnnotation.name() + " -> " + method.getName());
            }
        }
        
        Log.d(TAG, "扫描完成，共发现 " + tools.size() + " 个MCP工具");
        return tools;
    }
    
    /**
     * 创建工具信息对象
     */
    private static ToolInfo createToolInfo(Method method, MCPTool annotation) {
        ToolInfo info = new ToolInfo();
        info.name = annotation.name();
        info.description = annotation.description();
        info.returnType = annotation.returnType();
        info.version = annotation.version();
        info.method = method;
        info.parameters = extractParameterInfo(method);
        
        return info;
    }
    
    /**
     * 提取方法参数信息
     */
    private static List<ParameterInfo> extractParameterInfo(Method method) {
        List<ParameterInfo> parameterInfos = new ArrayList<>();
        Parameter[] parameters = method.getParameters();
        
        for (Parameter parameter : parameters) {
            MCPParam paramAnnotation = parameter.getAnnotation(MCPParam.class);
            if (paramAnnotation != null) {
                ParameterInfo paramInfo = new ParameterInfo();
                paramInfo.name = paramAnnotation.name();
                paramInfo.type = paramAnnotation.type();
                paramInfo.description = paramAnnotation.description();
                paramInfo.required = paramAnnotation.required();
                paramInfo.defaultValue = paramAnnotation.defaultValue();
                paramInfo.javaType = parameter.getType();
                
                parameterInfos.add(paramInfo);
            }
        }
        
        return parameterInfos;
    }
    
    /**
     * 工具信息类
     */
    public static class ToolInfo {
        public String name;
        public String description;
        public MCPDataType returnType;
        public String version;
        public Method method;
        public List<ParameterInfo> parameters;
    }
    
    /**
     * 参数信息类
     */
    public static class ParameterInfo {
        public String name;
        public MCPDataType type;
        public String description;
        public boolean required;
        public String defaultValue;
        public Class<?> javaType;
    }
}
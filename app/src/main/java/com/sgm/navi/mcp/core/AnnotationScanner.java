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
     * 根据Java类型自动推断MCPDataType
     * 与新SDK保持一致：不依赖注解type字段，完全基于Java反射
     */
    private static MCPDataType inferMCPDataType(Class<?> javaType) {
        if (javaType == String.class) {
            return MCPDataType.STRING;
        } else if (javaType == int.class || javaType == Integer.class) {
            return MCPDataType.INTEGER;
        } else if (javaType == long.class || javaType == Long.class ||
                   javaType == double.class || javaType == Double.class ||
                   javaType == float.class || javaType == Float.class) {
            return MCPDataType.NUMBER;
        } else if (javaType == boolean.class || javaType == Boolean.class) {
            return MCPDataType.BOOLEAN;
        } else if (javaType.isArray() || List.class.isAssignableFrom(javaType)) {
            return MCPDataType.ARRAY;
        } else {
            return MCPDataType.OBJECT;
        }
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
                // 使用自动类型推断，不再依赖注解type字段
                paramInfo.type = inferMCPDataType(parameter.getType());
                paramInfo.description = paramAnnotation.description();
                paramInfo.required = paramAnnotation.required();
                paramInfo.defaultValue = paramAnnotation.defaultValue();
                paramInfo.javaType = parameter.getType();
                
                parameterInfos.add(paramInfo);
                
                // 调试日志：显示类型推断结果
                Log.d(TAG, "参数 " + paramInfo.name + " - Java类型: " + parameter.getType().getSimpleName() + 
                      " -> MCP类型: " + paramInfo.type);
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
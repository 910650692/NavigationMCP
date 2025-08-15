package com.sgm.navi.mcp.core;

import android.util.Log;
import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

import java.lang.reflect.Method;
import java.util.List;

/**
 * MCP工具反射调用引擎
 * 负责通过反射调用工具方法
 */
public class ReflectionInvoker {
    private static final String TAG = "ReflectionInvoker";
    private static final Gson gson = new Gson();

    /**
     * 调用MCP工具
     * @param toolInstance 工具实例
     * @param toolInfo 工具信息
     * @param parametersJson 参数JSON字符串
     * @return 工具执行结果
     */
    public static String invokeTool(Object toolInstance, AnnotationScanner.ToolInfo toolInfo, String parametersJson) {
        try {
            Method method = toolInfo.method;
            method.setAccessible(true);
            
            // 解析参数
            Object[] args = parseParameters(toolInfo.parameters, parametersJson);
            
            // 调用方法
            Object result = method.invoke(toolInstance, args);
            
            // 处理返回值
            if (result == null) {
                return "null";
            } else if (result instanceof String) {
                return (String) result;
            } else {
                return gson.toJson(result);
            }
            
        } catch (Exception e) {
            Log.e(TAG, "调用工具失败: " + toolInfo.name, e);
            JsonObject error = new JsonObject();
            error.addProperty("error", "工具调用失败: " + e.getMessage());
            return gson.toJson(error);
        }
    }
    
    /**
     * 解析参数
     */
    private static Object[] parseParameters(List<AnnotationScanner.ParameterInfo> parameterInfos, String parametersJson) {
        if (parameterInfos.isEmpty()) {
            return new Object[0];
        }
        
        JsonObject params = new JsonObject();
        if (parametersJson != null && !parametersJson.trim().isEmpty()) {
            try {
                params = JsonParser.parseString(parametersJson).getAsJsonObject();
            } catch (Exception e) {
                Log.w(TAG, "解析参数JSON失败: " + e.getMessage());
            }
        }
        
        Object[] args = new Object[parameterInfos.size()];
        
        for (int i = 0; i < parameterInfos.size(); i++) {
            AnnotationScanner.ParameterInfo paramInfo = parameterInfos.get(i);
            args[i] = extractParameterValue(params, paramInfo);
        }
        
        return args;
    }
    
    /**
     * 提取参数值
     */
    private static Object extractParameterValue(JsonObject params, AnnotationScanner.ParameterInfo paramInfo) {
        String paramName = paramInfo.name;
        
        if (!params.has(paramName)) {
            if (paramInfo.required) {
                Log.w(TAG, "缺少必需参数: " + paramName);
                return getDefaultValue(paramInfo);
            } else {
                return getDefaultValue(paramInfo);
            }
        }
        
        try {
            switch (paramInfo.type) {
                case STRING:
                    return params.get(paramName).getAsString();
                case NUMBER:
                    if (paramInfo.javaType == int.class || paramInfo.javaType == Integer.class) {
                        return params.get(paramName).getAsInt();
                    } else if (paramInfo.javaType == double.class || paramInfo.javaType == Double.class) {
                        return params.get(paramName).getAsDouble();
                    } else if (paramInfo.javaType == float.class || paramInfo.javaType == Float.class) {
                        return params.get(paramName).getAsFloat();
                    } else if (paramInfo.javaType == long.class || paramInfo.javaType == Long.class) {
                        return params.get(paramName).getAsLong();
                    } else {
                        return params.get(paramName).getAsString();
                    }
                case BOOLEAN:
                    return params.get(paramName).getAsBoolean();
                case OBJECT:
                case ARRAY:
                default:
                    return params.get(paramName).toString();
            }
        } catch (Exception e) {
            Log.w(TAG, "参数类型转换失败: " + paramName + ", " + e.getMessage());
            return getDefaultValue(paramInfo);
        }
    }
    
    /**
     * 获取默认值
     */
    private static Object getDefaultValue(AnnotationScanner.ParameterInfo paramInfo) {
        if (!paramInfo.defaultValue.isEmpty()) {
            return paramInfo.defaultValue;
        }
        
        // 根据Java类型返回默认值
        Class<?> type = paramInfo.javaType;
        if (type == String.class) {
            return "";
        } else if (type == int.class || type == Integer.class) {
            return 0;
        } else if (type == double.class || type == Double.class) {
            return 0.0;
        } else if (type == boolean.class || type == Boolean.class) {
            return false;
        } else {
            return null;
        }
    }
}
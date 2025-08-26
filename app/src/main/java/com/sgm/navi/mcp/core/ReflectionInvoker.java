package com.sgm.navi.mcp.core;

import android.util.Log;
import com.google.gson.Gson;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.gson.JsonPrimitive;

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
        Log.d(TAG, "开始调用工具: " + toolInfo.name + ", 参数: " + parametersJson);
        
        try {
            Method method = toolInfo.method;
            method.setAccessible(true);
            
            // 检查参数格式并收集警告
            java.util.List<String> warnings = validateParameterFormat(toolInfo, parametersJson);
            
            // 解析参数
            Object[] args = parseParameters(toolInfo.parameters, parametersJson);
            
            // 记录解析后的参数信息
            StringBuilder argsLog = new StringBuilder();
            for (int i = 0; i < args.length; i++) {
                if (i > 0) argsLog.append(", ");
                AnnotationScanner.ParameterInfo paramInfo = toolInfo.parameters.get(i);
                argsLog.append(paramInfo.name).append("=")
                       .append(args[i]).append("(").append(args[i] != null ? args[i].getClass().getSimpleName() : "null").append(")");
            }
            Log.d(TAG, "解析后的参数: " + argsLog.toString());
            
            // 调用方法
            Object result = method.invoke(toolInstance, args);
            
            Log.d(TAG, "工具调用成功: " + toolInfo.name);
            
            // 处理返回值
            String resultStr;
            if (result == null) {
                resultStr = "null";
            } else if (result instanceof String) {
                resultStr = (String) result;
            } else {
                resultStr = gson.toJson(result);
            }
            
            // 如果有警告，包装结果以包含警告信息
            if (!warnings.isEmpty()) {
                Log.i(TAG, "工具 " + toolInfo.name + " 执行成功但有警告: " + warnings);
                
                JsonObject wrappedResult = new JsonObject();
                // 尝试解析原始结果为JSON
                try {
                    JsonElement originalResult = JsonParser.parseString(resultStr);
                    wrappedResult.add("result", originalResult);
                } catch (Exception e) {
                    // 如果原始结果不是JSON，直接作为字符串存储
                    wrappedResult.addProperty("result", resultStr);
                }
                wrappedResult.add("warnings", gson.toJsonTree(warnings));
                wrappedResult.addProperty("status", "success_with_warnings");
                return gson.toJson(wrappedResult);
            } else {
                return resultStr;
            }
            
        } catch (IllegalArgumentException e) {
            // 特殊处理参数类型不匹配的错误
            Log.e(TAG, "参数类型不匹配: " + toolInfo.name + ", " + e.getMessage(), e);
            
            JsonObject error = new JsonObject();
            error.addProperty("status", "error");
            error.addProperty("error_type", "PARAMETER_TYPE_MISMATCH");
            error.addProperty("error", "参数类型不匹配，请检查参数格式");
            error.addProperty("tool_name", toolInfo.name);
            error.addProperty("message", e.getMessage());
            error.addProperty("received_params", parametersJson);
            
            // 生成正确的调用示例
            JsonObject correctExample = generateCorrectExample(toolInfo);
            error.add("correct_example", correctExample);
            
            // 添加详细的参数规范
            JsonObject parameterSpecs = new JsonObject();
            for (AnnotationScanner.ParameterInfo paramInfo : toolInfo.parameters) {
                JsonObject paramSpec = new JsonObject();
                paramSpec.addProperty("type", paramInfo.type.toString());
                paramSpec.addProperty("java_type", paramInfo.javaType.getSimpleName());
                paramSpec.addProperty("required", paramInfo.required);
                paramSpec.addProperty("description", paramInfo.description);
                paramSpec.addProperty("example", getExampleValue(paramInfo));
                if (!paramInfo.defaultValue.isEmpty()) {
                    paramSpec.addProperty("default", paramInfo.defaultValue);
                }
                parameterSpecs.add(paramInfo.name, paramSpec);
            }
            error.add("parameter_specifications", parameterSpecs);
            
            // 添加修正建议
            error.addProperty("suggestion", "请使用JSON对象格式传递参数，确保参数类型正确。所有参数都应该是JSON对象的直接属性，不要嵌套在其他对象中。");
            
            return gson.toJson(error);
            
        } catch (Exception e) {
            Log.e(TAG, "调用工具失败: " + toolInfo.name, e);
            
            JsonObject error = new JsonObject();
            error.addProperty("status", "error");
            error.addProperty("error_type", "TOOL_EXECUTION_ERROR");
            error.addProperty("error", "工具调用失败: " + e.getMessage());
            error.addProperty("tool_name", toolInfo.name);
            error.addProperty("exception_type", e.getClass().getSimpleName());
            error.addProperty("received_params", parametersJson);
            
            // 如果是参数相关的错误，也提供正确示例
            if (e.getMessage() != null && (e.getMessage().contains("参数") || e.getMessage().contains("parameter") || e.getMessage().contains("argument"))) {
                JsonObject correctExample = generateCorrectExample(toolInfo);
                error.add("correct_example", correctExample);
                error.addProperty("suggestion", "请检查参数格式是否正确，参考correct_example中的示例格式。");
            }
            
            return gson.toJson(error);
        }
    }
    
    /**
     * 验证参数格式并收集警告信息
     */
    private static java.util.List<String> validateParameterFormat(AnnotationScanner.ToolInfo toolInfo, String parametersJson) {
        java.util.List<String> warnings = new java.util.ArrayList<>();
        
        if (parametersJson == null || parametersJson.trim().isEmpty()) {
            return warnings;
        }
        
        try {
            JsonObject params = JsonParser.parseString(parametersJson).getAsJsonObject();
            
            // 获取所有期望的参数名
            java.util.Set<String> expectedParams = new java.util.HashSet<>();
            for (AnnotationScanner.ParameterInfo paramInfo : toolInfo.parameters) {
                expectedParams.add(paramInfo.name);
            }
            
            // 检查是否有额外的参数
            java.util.Set<String> unexpectedParams = new java.util.HashSet<>();
            for (String key : params.keySet()) {
                if (!expectedParams.contains(key)) {
                    unexpectedParams.add(key);
                }
            }
            
            // 智能宽松模式：记录警告但继续执行
            if (!unexpectedParams.isEmpty()) {
                String warningMsg = "工具 " + toolInfo.name + " 包含未声明的参数: " + unexpectedParams + "，这些参数将被忽略";
                Log.w(TAG, warningMsg);
                
                StringBuilder suggestion = new StringBuilder("包含未声明的参数: " + unexpectedParams + "，已自动忽略。");
                suggestion.append("有效参数列表: ");
                for (AnnotationScanner.ParameterInfo paramInfo : toolInfo.parameters) {
                    suggestion.append(paramInfo.name).append("(").append(paramInfo.required ? "必需" : "可选").append(") ");
                }
                warnings.add(suggestion.toString());
            }
            
        } catch (Exception e) {
            Log.w(TAG, "验证参数格式时出错: " + e.getMessage());
            warnings.add("参数格式解析出现问题: " + e.getMessage());
        }
        
        return warnings;
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
                case INTEGER:
                    JsonElement intElement = params.get(paramName);
                    if (intElement.isJsonPrimitive()) {
                        JsonPrimitive intPrimitive = intElement.getAsJsonPrimitive();
                        if (intPrimitive.isNumber()) {
                            return intPrimitive.getAsInt();
                        } else if (intPrimitive.isString()) {
                            // 支持字符串到整数的转换
                            try {
                                return Integer.parseInt(intPrimitive.getAsString().trim());
                            } catch (NumberFormatException e) {
                                Log.w(TAG, "无法将字符串 '" + intPrimitive.getAsString() + "' 转换为整数");
                                return 0;
                            }
                        }
                    }
                    return intElement.getAsInt();
                case NUMBER:
                    JsonElement numElement = params.get(paramName);
                    if (paramInfo.javaType == int.class || paramInfo.javaType == Integer.class) {
                        if (numElement.isJsonPrimitive() && numElement.getAsJsonPrimitive().isString()) {
                            try {
                                return Integer.parseInt(numElement.getAsString().trim());
                            } catch (NumberFormatException e) {
                                Log.w(TAG, "无法将字符串转换为整数: " + numElement.getAsString());
                                return 0;
                            }
                        }
                        return numElement.getAsInt();
                    } else if (paramInfo.javaType == double.class || paramInfo.javaType == Double.class) {
                        if (numElement.isJsonPrimitive() && numElement.getAsJsonPrimitive().isString()) {
                            try {
                                return Double.parseDouble(numElement.getAsString().trim());
                            } catch (NumberFormatException e) {
                                Log.w(TAG, "无法将字符串转换为双精度: " + numElement.getAsString());
                                return 0.0;
                            }
                        }
                        return numElement.getAsDouble();
                    } else if (paramInfo.javaType == float.class || paramInfo.javaType == Float.class) {
                        if (numElement.isJsonPrimitive() && numElement.getAsJsonPrimitive().isString()) {
                            try {
                                return Float.parseFloat(numElement.getAsString().trim());
                            } catch (NumberFormatException e) {
                                Log.w(TAG, "无法将字符串转换为浮点数: " + numElement.getAsString());
                                return 0.0f;
                            }
                        }
                        return numElement.getAsFloat();
                    } else if (paramInfo.javaType == long.class || paramInfo.javaType == Long.class) {
                        if (numElement.isJsonPrimitive() && numElement.getAsJsonPrimitive().isString()) {
                            try {
                                return Long.parseLong(numElement.getAsString().trim());
                            } catch (NumberFormatException e) {
                                Log.w(TAG, "无法将字符串转换为长整数: " + numElement.getAsString());
                                return 0L;
                            }
                        }
                        return numElement.getAsLong();
                    } else {
                        return numElement.getAsString();
                    }
                case BOOLEAN:
                    JsonElement element = params.get(paramName);
                    if (element.isJsonPrimitive()) {
                        JsonPrimitive primitive = element.getAsJsonPrimitive();
                        if (primitive.isBoolean()) {
                            return primitive.getAsBoolean();
                        } else if (primitive.isString()) {
                            // 支持字符串到布尔的转换
                            String strValue = primitive.getAsString().toLowerCase().trim();
                            // 明确处理 true 值
                            if ("true".equals(strValue) || "1".equals(strValue) || "yes".equals(strValue)) {
                                return true;
                            }
                            // 明确处理 false 值
                            if ("false".equals(strValue) || "0".equals(strValue) || "no".equals(strValue)) {
                                return false;
                            }
                            // 对于其他值，记录警告并返回默认值
                            Log.w(TAG, "无法识别的布尔值: '" + strValue + "'，使用默认值 false");
                            return false;
                        }
                    }
                    return element.getAsBoolean();
                case OBJECT:
                case ARRAY:
                default:
                    return params.get(paramName).toString();
            }
        } catch (Exception e) {
            Log.e(TAG, "参数类型转换失败: " + paramName + " (期望类型: " + paramInfo.javaType.getSimpleName() + 
                      ", 实际值: " + params.get(paramName) + "), " + e.getMessage());
            return getDefaultValue(paramInfo);
        }
    }
    
    /**
     * 获取默认值
     */
    private static Object getDefaultValue(AnnotationScanner.ParameterInfo paramInfo) {
        // 根据参数类型和默认值字符串，返回正确类型的默认值
        Class<?> type = paramInfo.javaType;
        String defaultValueStr = paramInfo.defaultValue;
        
        try {
            // 如果有默认值字符串，尝试转换为对应类型
            if (!defaultValueStr.isEmpty()) {
                if (type == String.class) {
                    return defaultValueStr;
                } else if (type == int.class || type == Integer.class) {
                    return Integer.parseInt(defaultValueStr);
                } else if (type == double.class || type == Double.class) {
                    return Double.parseDouble(defaultValueStr);
                } else if (type == float.class || type == Float.class) {
                    return Float.parseFloat(defaultValueStr);
                } else if (type == long.class || type == Long.class) {
                    return Long.parseLong(defaultValueStr);
                } else if (type == boolean.class || type == Boolean.class) {
                    return Boolean.parseBoolean(defaultValueStr);
                } else {
                    return defaultValueStr;
                }
            }
        } catch (NumberFormatException e) {
            Log.w(TAG, "解析默认值失败: " + paramInfo.name + " = " + defaultValueStr + ", " + e.getMessage());
        }
        
        // 如果没有默认值或解析失败，根据Java类型返回默认值
        if (type == String.class) {
            return "";
        } else if (type == int.class || type == Integer.class) {
            return 0;
        } else if (type == double.class || type == Double.class) {
            return 0.0;
        } else if (type == float.class || type == Float.class) {
            return 0.0f;
        } else if (type == long.class || type == Long.class) {
            return 0L;
        } else if (type == boolean.class || type == Boolean.class) {
            return false;
        } else {
            return null;
        }
    }
    
    /**
     * 生成正确的调用示例
     */
    private static JsonObject generateCorrectExample(AnnotationScanner.ToolInfo toolInfo) {
        JsonObject example = new JsonObject();
        example.addProperty("tool_name", toolInfo.name);
        
        JsonObject parameters = new JsonObject();
        for (AnnotationScanner.ParameterInfo paramInfo : toolInfo.parameters) {
            String exampleValue = getExampleValue(paramInfo);
            
            // 根据参数类型添加正确格式的示例值
            try {
                switch (paramInfo.type) {
                    case INTEGER:
                        parameters.addProperty(paramInfo.name, Integer.parseInt(exampleValue));
                        break;
                    case NUMBER:
                        if (paramInfo.javaType == String.class) {
                            // 对于NUMBER类型但Java类型是String的参数，保持字符串格式
                            parameters.addProperty(paramInfo.name, exampleValue);
                        } else if (paramInfo.javaType == double.class || paramInfo.javaType == Double.class) {
                            parameters.addProperty(paramInfo.name, Double.parseDouble(exampleValue));
                        } else if (paramInfo.javaType == float.class || paramInfo.javaType == Float.class) {
                            parameters.addProperty(paramInfo.name, Float.parseFloat(exampleValue));
                        } else {
                            parameters.addProperty(paramInfo.name, exampleValue);
                        }
                        break;
                    case BOOLEAN:
                        parameters.addProperty(paramInfo.name, Boolean.parseBoolean(exampleValue));
                        break;
                    case STRING:
                    default:
                        parameters.addProperty(paramInfo.name, exampleValue);
                        break;
                }
            } catch (NumberFormatException e) {
                // 如果解析失败，回退到字符串格式
                parameters.addProperty(paramInfo.name, exampleValue);
            }
        }
        
        example.add("parameters", parameters);
        return example;
    }
    
    /**
     * 根据参数信息生成示例值（用于错误提示）
     */
    private static String getExampleValue(AnnotationScanner.ParameterInfo paramInfo) {
        // 优先使用默认值作为示例
        if (!paramInfo.defaultValue.isEmpty()) {
            return paramInfo.defaultValue;
        }
        
        // 根据参数类型生成示例值
        Class<?> type = paramInfo.javaType;
        if (type == String.class) {
            // 根据参数名推测示例值
            String name = paramInfo.name.toLowerCase();
            if (name.contains("keyword")) {
                return "餐厅";
            } else if (name.contains("sort")) {
                return "DISTANCE_ASC";
            } else {
                return "example";
            }
        } else if (type == int.class || type == Integer.class) {
            if (paramInfo.name.toLowerCase().contains("radius")) {
                return "1000";
            } else if (paramInfo.name.toLowerCase().contains("page")) {
                return "1";
            } else {
                return "0";
            }
        } else if (type == double.class || type == Double.class) {
            return "0.0";
        } else if (type == boolean.class || type == Boolean.class) {
            return "false";
        } else {
            return "null";
        }
    }
}
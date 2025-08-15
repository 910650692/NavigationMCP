package com.sgm.navi.mcp.core;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;

import java.util.List;

/**
 * MCP工具Schema生成器
 * 负责根据工具信息生成符合MCP标准的Schema
 */
public class SchemaGenerator {
    
    /**
     * 为工具生成MCP Schema
     * @param toolInfo 工具信息
     * @return JSON Schema字符串
     */
    public static String generateSchema(AnnotationScanner.ToolInfo toolInfo) {
        JsonObject schema = new JsonObject();
        schema.addProperty("type", "function");
        schema.add("function", createFunctionSchema(toolInfo));
        
        return schema.toString();
    }
    
    /**
     * 创建函数Schema
     */
    private static JsonObject createFunctionSchema(AnnotationScanner.ToolInfo toolInfo) {
        JsonObject function = new JsonObject();
        function.addProperty("name", toolInfo.name);
        function.addProperty("description", toolInfo.description);
        
        // 参数Schema
        JsonObject parameters = new JsonObject();
        parameters.addProperty("type", "object");
        
        JsonObject properties = new JsonObject();
        JsonArray required = new JsonArray();
        
        for (AnnotationScanner.ParameterInfo param : toolInfo.parameters) {
            JsonObject paramSchema = new JsonObject();
            paramSchema.addProperty("type", param.type.getType());
            paramSchema.addProperty("description", param.description);
            
            if (!param.defaultValue.isEmpty()) {
                paramSchema.addProperty("default", param.defaultValue);
            }
            
            properties.add(param.name, paramSchema);
            
            if (param.required) {
                required.add(param.name);
            }
        }
        
        parameters.add("properties", properties);
        if (required.size() > 0) {
            parameters.add("required", required);
        }
        
        function.add("parameters", parameters);
        return function;
    }
}
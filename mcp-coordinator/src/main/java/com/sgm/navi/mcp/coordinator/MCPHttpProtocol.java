package com.sgm.navi.mcp.coordinator;

import com.google.gson.JsonObject;

/**
 * MCP HTTP协议适配类
 * 定义MCP协议的标准消息格式和常量
 */
public class MCPHttpProtocol {
    
    // JSON-RPC 2.0版本
    public static final String JSONRPC_VERSION = "2.0";
    
    // MCP协议版本
    public static final String MCP_PROTOCOL_VERSION = "2024-11-05";
    
    // 支持的MCP方法
    public static class Methods {
        public static final String INITIALIZE = "initialize";
        public static final String TOOLS_LIST = "tools/list";
        public static final String TOOLS_CALL = "tools/call";
        public static final String RESOURCES_LIST = "resources/list";
        public static final String RESOURCES_READ = "resources/read";
        public static final String PROMPTS_LIST = "prompts/list";
        public static final String PROMPTS_GET = "prompts/get";
    }
    
    // 标准错误代码
    public static class ErrorCodes {
        public static final int PARSE_ERROR = -32700;
        public static final int INVALID_REQUEST = -32600;
        public static final int METHOD_NOT_FOUND = -32601;
        public static final int INVALID_PARAMS = -32602;
        public static final int INTERNAL_ERROR = -32603;
        
        // MCP特定错误代码
        public static final int TOOL_NOT_FOUND = -32000;
        public static final int TOOL_EXECUTION_ERROR = -32001;
        public static final int SERVICE_NOT_AVAILABLE = -32002;
        public static final int TIMEOUT_ERROR = -32003;
    }
    
    /**
     * 创建标准的MCP初始化响应
     */
    public static JsonObject createInitializeResponse() {
        JsonObject capabilities = new JsonObject();
        
        // 工具能力
        JsonObject tools = new JsonObject();
        tools.addProperty("listChanged", true);
        capabilities.add("tools", tools);
        
        // 服务器信息
        JsonObject serverInfo = new JsonObject();
        serverInfo.addProperty("name", "SGM Navigation MCP Coordinator");
        serverInfo.addProperty("version", "1.0.0");
        
        JsonObject response = new JsonObject();
        response.addProperty("protocolVersion", MCP_PROTOCOL_VERSION);
        response.add("capabilities", capabilities);
        response.add("serverInfo", serverInfo);
        
        return response;
    }
    
    /**
     * 创建工具调用成功响应
     */
    public static JsonObject createToolCallResponse(String content) {
        JsonObject response = new JsonObject();
        response.addProperty("content", content);
        response.addProperty("isText", true);
        return response;
    }
    
    /**
     * 创建工具列表响应
     */
    public static JsonObject createToolsListResponse(ToolInfo[] tools) {
        JsonObject response = new JsonObject();
        
        // 将工具信息转换为MCP格式
        JsonObject[] toolObjects = new JsonObject[tools.length];
        for (int i = 0; i < tools.length; i++) {
            toolObjects[i] = tools[i].toJsonObject();
        }
        
        response.add("tools", com.google.gson.JsonParser.parseString(
            new com.google.gson.Gson().toJson(toolObjects)));
        
        return response;
    }
    
    /**
     * 验证JSON-RPC请求格式
     */
    public static boolean isValidJsonRpcRequest(JsonObject request) {
        return request != null &&
               request.has("jsonrpc") &&
               JSONRPC_VERSION.equals(request.get("jsonrpc").getAsString()) &&
               request.has("method") &&
               request.has("id");
    }
    
    /**
     * 获取错误消息描述
     */
    public static String getErrorMessage(int errorCode) {
        switch (errorCode) {
            case ErrorCodes.PARSE_ERROR:
                return "Parse error";
            case ErrorCodes.INVALID_REQUEST:
                return "Invalid Request";
            case ErrorCodes.METHOD_NOT_FOUND:
                return "Method not found";
            case ErrorCodes.INVALID_PARAMS:
                return "Invalid params";
            case ErrorCodes.INTERNAL_ERROR:
                return "Internal error";
            case ErrorCodes.TOOL_NOT_FOUND:
                return "Tool not found";
            case ErrorCodes.TOOL_EXECUTION_ERROR:
                return "Tool execution error";
            case ErrorCodes.SERVICE_NOT_AVAILABLE:
                return "Service not available";
            case ErrorCodes.TIMEOUT_ERROR:
                return "Request timeout";
            default:
                return "Unknown error";
        }
    }
    
    /**
     * 工具信息类
     */
    public static class ToolInfo {
        private String name;
        private String description;
        private JsonObject inputSchema;
        
        public ToolInfo(String name, String description, JsonObject inputSchema) {
            this.name = name;
            this.description = description;
            this.inputSchema = inputSchema;
        }
        
        public JsonObject toJsonObject() {
            JsonObject toolObj = new JsonObject();
            toolObj.addProperty("name", name);
            toolObj.addProperty("description", description);
            if (inputSchema != null) {
                toolObj.add("inputSchema", inputSchema);
            }
            return toolObj;
        }
        
        public String getName() { return name; }
        public String getDescription() { return description; }
        public JsonObject getInputSchema() { return inputSchema; }
    }
}
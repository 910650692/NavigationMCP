package com.sgm.navi.mcp.coordinator;

import android.os.RemoteException;
import android.util.Log;
import com.google.gson.Gson;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.sgm.navi.mcp.IMCPService;
import com.sgm.navi.mcp.IToolCallback;
import com.sgm.navi.mcp.ToolRequest;
import com.sgm.navi.mcp.ToolResponse;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

/**
 * JSON-RPC请求处理器
 * 负责解析JSON-RPC请求并转换为AIDL调用
 */
public class JsonRpcHandler {
    private static final String TAG = "JsonRpcHandler";
    private final Gson gson = new Gson();
    private final JsonParser jsonParser = new JsonParser();
    private IMCPService mcpService;
    
    public JsonRpcHandler(IMCPService mcpService) {
        this.mcpService = mcpService;
    }
    
    /**
     * 处理JSON-RPC请求
     */
    public String handleRequest(String jsonRequest) {
        try {
            Log.i(TAG, "=== 开始解析JSON-RPC请求 ===");
            Log.i(TAG, "原始请求: " + (jsonRequest != null ? jsonRequest : "null"));
            
            if (jsonRequest == null || jsonRequest.trim().isEmpty()) {
                Log.e(TAG, "❌ 请求体为空");
                return createErrorResponse(null, -32700, "Empty request body");
            }
            
            // 解析JSON-RPC请求
            JsonObject requestObj = jsonParser.parse(jsonRequest).getAsJsonObject();
            Log.i(TAG, "✅ JSON解析成功");
            
            // 打印解析后的请求结构
            Log.i(TAG, "请求对象keys: " + requestObj.keySet());
            if (requestObj.has("jsonrpc")) {
                Log.i(TAG, "jsonrpc版本: " + requestObj.get("jsonrpc").getAsString());
            }
            if (requestObj.has("method")) {
                Log.i(TAG, "请求方法: " + requestObj.get("method").getAsString());
            }
            if (requestObj.has("id")) {
                Log.i(TAG, "请求ID: " + requestObj.get("id"));
            }
            if (requestObj.has("params")) {
                Log.i(TAG, "请求参数: " + requestObj.get("params"));
            }
            
            // 验证JSON-RPC格式
            if (!isValidJsonRpcRequest(requestObj)) {
                Log.e(TAG, "❌ 无效的JSON-RPC请求格式");
                return createErrorResponse(null, -32600, "Invalid Request - missing jsonrpc/method/id");
            }
            
            String method = requestObj.get("method").getAsString();
            JsonElement paramsElement = requestObj.get("params");
            JsonElement idElement = requestObj.get("id");
            
            Log.i(TAG, "✅ 处理JSON-RPC请求: method=" + method + ", id=" + 
                (idElement != null ? idElement.toString() : "null"));
            
            // 根据方法类型处理请求
            String response;
            switch (method) {
                case "tools/call":
                    Log.i(TAG, "路由到 tools/call 处理器");
                    response = handleToolCall(paramsElement, idElement);
                    break;
                case "tools/list":
                    Log.i(TAG, "路由到 tools/list 处理器");
                    response = handleToolsList(idElement);
                    break;
                case "initialize":
                    Log.i(TAG, "路由到 initialize 处理器");
                    response = handleInitialize(paramsElement, idElement);
                    break;
                case "notifications/initialized":
                    Log.i(TAG, "处理 notifications/initialized 通知");
                    response = handleNotificationInitialized();
                    break;
                case "ping":
                    Log.i(TAG, "处理 ping 请求");
                    response = handlePing(idElement);
                    break;
                default:
                    Log.w(TAG, "❌ 未知方法: " + method);
                    response = createErrorResponse(idElement, -32601, "Method not found: " + method);
                    break;
            }
            
            Log.i(TAG, "✅ 请求处理完成，响应长度: " + (response != null ? response.length() : 0));
            return response;
            
        } catch (Exception e) {
            Log.e(TAG, "❌ 处理JSON-RPC请求失败", e);
            Log.e(TAG, "异常详情: " + e.getClass().getSimpleName() + " - " + e.getMessage());
            return createErrorResponse(null, -32700, "Parse error: " + e.getMessage());
        } finally {
            Log.i(TAG, "=== JSON-RPC请求处理结束 ===");
        }
    }
    
    /**
     * 验证是否为有效的JSON-RPC请求
     */
    private boolean isValidJsonRpcRequest(JsonObject request) {
        if (!request.has("jsonrpc") || !"2.0".equals(request.get("jsonrpc").getAsString()) || !request.has("method")) {
            return false;
        }
        
        String method = request.get("method").getAsString();
        // 通知请求（notifications/）不需要id字段
        if (method.startsWith("notifications/")) {
            return true;
        }
        
        // 其他请求需要id字段
        return request.has("id");
    }
    
    /**
     * 处理工具调用请求
     */
    private String handleToolCall(JsonElement paramsElement, JsonElement idElement) {
        try {
            if (paramsElement == null || !paramsElement.isJsonObject()) {
                return createErrorResponse(idElement, -32602, "Invalid params for tools/call");
            }
            
            JsonObject params = paramsElement.getAsJsonObject();
            
            if (!params.has("name") || !params.has("arguments")) {
                return createErrorResponse(idElement, -32602, "Missing 'name' or 'arguments' in params");
            }
            
            String toolName = params.get("name").getAsString();
            JsonObject arguments = params.get("arguments").getAsJsonObject();
            
            Log.d(TAG, "调用工具: " + toolName + ", 原始参数: " + arguments.toString());
            
            // 检查是否有params包装，如果有则解包
            JsonObject actualArguments = arguments;
            if (arguments.has("params") && arguments.get("params").isJsonObject()) {
                actualArguments = arguments.get("params").getAsJsonObject();
                Log.d(TAG, "检测到params包装，解包后参数: " + actualArguments.toString());
            }
            
            // 参数名映射：统一不同客户端的参数名
            JsonObject mappedArguments = mapToolArguments(toolName, actualArguments);
            Log.d(TAG, "参数映射后: " + mappedArguments.toString());
            
            // 创建工具请求
            ToolRequest toolRequest = new ToolRequest(toolName, mappedArguments.toString());
            
            // 同步调用工具
            ToolCallResult result = callToolSync(toolRequest);
            
            if (result.success) {
                // 创建符合MCP标准的成功响应
                JsonObject responseData = new JsonObject();
                
                // content应该是数组格式，包含文本内容对象
                com.google.gson.JsonArray contentArray = new com.google.gson.JsonArray();
                JsonObject textContent = new JsonObject();
                textContent.addProperty("type", "text");
                textContent.addProperty("text", result.response.getResult());
                contentArray.add(textContent);
                
                responseData.add("content", contentArray);
                responseData.addProperty("isError", false);
                
                return createSuccessResponse(idElement, responseData);
            } else {
                return createErrorResponse(idElement, -32000, "Tool call failed: " + result.error);
            }
            
        } catch (Exception e) {
            Log.e(TAG, "处理工具调用请求失败", e);
            return createErrorResponse(idElement, -32603, "Internal error: " + e.getMessage());
        }
    }
    
    /**
     * 处理工具列表请求
     */
    private String handleToolsList(JsonElement idElement) {
        try {
            if (mcpService == null) {
                return createErrorResponse(idElement, -32002, "MCP service not available");
            }
            
            String[] toolNames = mcpService.getRegisteredTools();
            
            // 构造符合MCP标准的工具列表响应
            JsonObject responseData = new JsonObject();
            com.google.gson.JsonArray toolsArray = new com.google.gson.JsonArray();
            
            // 为每个工具创建完整的schema - 通用化处理
            for (String toolName : toolNames) {
                JsonObject toolSchema = new JsonObject();
                toolSchema.addProperty("name", toolName);
                toolSchema.addProperty("description", "Navigation tool: " + toolName);
                
                // 添加通用的输入schema
                JsonObject inputSchema = new JsonObject();
                inputSchema.addProperty("type", "object");
                
                // 所有工具都可能有通用参数，具体的schema应该由工具提供者定义
                JsonObject properties = new JsonObject();
                JsonObject paramsProp = new JsonObject();
                paramsProp.addProperty("type", "object");
                paramsProp.addProperty("description", "Tool-specific parameters");
                properties.add("params", paramsProp);
                
                inputSchema.add("properties", properties);
                toolSchema.add("inputSchema", inputSchema);
                
                toolsArray.add(toolSchema);
            }
            
            responseData.add("tools", toolsArray);
            
            Log.d(TAG, "返回标准MCP工具列表，共 " + toolNames.length + " 个工具");
            
            return createSuccessResponse(idElement, responseData);
            
        } catch (RemoteException e) {
            Log.e(TAG, "获取工具列表失败", e);
            return createErrorResponse(idElement, -32003, "Failed to get tools list: " + e.getMessage());
        }
    }
    
    /**
     * 处理初始化请求
     */
    private String handleInitialize(JsonElement paramsElement, JsonElement idElement) {
        Log.d(TAG, "处理初始化请求");
        
        // 构造初始化响应 - 按照MCP标准格式
        JsonObject responseData = new JsonObject();
        responseData.addProperty("protocolVersion", "2024-11-05");
        
        // serverInfo 应该是一个对象，不是字符串
        JsonObject serverInfo = new JsonObject();
        serverInfo.addProperty("name", "SGM Navigation MCP Coordinator");
        serverInfo.addProperty("version", "1.0");
        responseData.add("serverInfo", serverInfo);
        
        JsonObject capabilities = new JsonObject();
        JsonObject tools = new JsonObject();
        tools.addProperty("listChanged", true);
        capabilities.add("tools", tools);
        responseData.add("capabilities", capabilities);
        
        return createSuccessResponse(idElement, responseData);
    }
    
    /**
     * 处理 notifications/initialized 通知
     * 通知请求不需要响应，返回空字符串表示无需响应
     */
    private String handleNotificationInitialized() {
        Log.d(TAG, "收到初始化完成通知");
        // 通知请求不需要响应
        return "";
    }
    
    /**
     * 处理 ping 请求
     */
    private String handlePing(JsonElement idElement) {
        Log.d(TAG, "处理 ping 请求");
        
        // 构造简单的 pong 响应
        JsonObject responseData = new JsonObject();
        responseData.addProperty("result", "pong");
        
        return createSuccessResponse(idElement, responseData);
    }
    
    /**
     * 映射工具参数名，统一不同客户端的参数差异
     */
    private JsonObject mapToolArguments(String toolName, JsonObject originalArgs) {
        JsonObject mappedArgs = new JsonObject();
        
        // 复制所有原始参数
        for (String key : originalArgs.keySet()) {
            mappedArgs.add(key, originalArgs.get(key));
        }
        
        // 根据工具类型进行参数名映射
        switch (toolName) {
            case "search_poi":
                // 将 query 映射为 keyword
                if (originalArgs.has("query") && !originalArgs.has("keyword")) {
                    mappedArgs.add("keyword", originalArgs.get("query"));
                    Log.d(TAG, "search_poi: 映射 query -> keyword");
                }
                break;
                
            case "search_nearby":
                // 如果将来有参数名差异也可以在这里处理
                break;
                
            case "start_navigation":
                // destination 参数映射（如果需要的话）
                break;
        }
        
        return mappedArgs;
    }
    
    /**
     * 同步调用工具
     */
    private ToolCallResult callToolSync(ToolRequest request) {
        if (mcpService == null) {
            return new ToolCallResult(false, null, "MCP service not available");
        }
        
        final CountDownLatch latch = new CountDownLatch(1);
        final AtomicReference<ToolResponse> responseRef = new AtomicReference<>();
        final AtomicReference<String> errorRef = new AtomicReference<>();
        
        try {
            mcpService.callTool(request, new IToolCallback.Stub() {
                @Override
                public void onToolResult(ToolResponse response) throws RemoteException {
                    responseRef.set(response);
                    latch.countDown();
                }
                
                @Override
                public void onToolError(String error) throws RemoteException {
                    errorRef.set(error);
                    latch.countDown();
                }
            });
            
            // 等待结果，最多等待10秒
            boolean completed = latch.await(10, TimeUnit.SECONDS);
            
            if (!completed) {
                return new ToolCallResult(false, null, "Tool call timeout");
            }
            
            if (responseRef.get() != null) {
                return new ToolCallResult(true, responseRef.get(), null);
            } else {
                return new ToolCallResult(false, null, errorRef.get() != null ? errorRef.get() : "Unknown error");
            }
            
        } catch (Exception e) {
            Log.e(TAG, "同步调用工具失败", e);
            return new ToolCallResult(false, null, "Exception: " + e.getMessage());
        }
    }
    
    /**
     * 创建成功响应
     */
    private String createSuccessResponse(JsonElement id, JsonObject result) {
        JsonObject response = new JsonObject();
        response.addProperty("jsonrpc", "2.0");
        response.add("id", id);
        response.add("result", result);
        
        return gson.toJson(response);
    }
    
    /**
     * 创建错误响应
     */
    public String createErrorResponse(JsonElement id, int code, String message) {
        JsonObject error = new JsonObject();
        error.addProperty("code", code);
        error.addProperty("message", message);
        
        JsonObject response = new JsonObject();
        response.addProperty("jsonrpc", "2.0");
        response.add("id", id);
        response.add("error", error);
        
        return gson.toJson(response);
    }
    
    /**
     * 工具调用结果包装类
     */
    private static class ToolCallResult {
        final boolean success;
        final ToolResponse response;
        final String error;
        
        ToolCallResult(boolean success, ToolResponse response, String error) {
            this.success = success;
            this.response = response;
            this.error = error;
        }
    }
}
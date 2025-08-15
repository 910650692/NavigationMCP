package com.sgm.navi.mcp.coordinator;

import android.util.Log;
import fi.iki.elonen.NanoHTTPD;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * HTTP服务器管理类
 * 负责启动和管理MCP协调中心的HTTP服务器
 */
public class HttpServerManager extends NanoHTTPD {
    private static final String TAG = "HttpServerManager";
    private static final int DEFAULT_PORT = 8080;
    
    private JsonRpcHandler jsonRpcHandler;
    private boolean isStarted = false;
    
    public HttpServerManager(JsonRpcHandler jsonRpcHandler) {
        this(DEFAULT_PORT, jsonRpcHandler);
    }
    
    public HttpServerManager(int port, JsonRpcHandler jsonRpcHandler) {
        super(port);
        this.jsonRpcHandler = jsonRpcHandler;
    }
    
    /**
     * 启动HTTP服务器
     */
    public boolean startServer() {
        try {
            start(NanoHTTPD.SOCKET_READ_TIMEOUT, false);
            isStarted = true;
            Log.d(TAG, "✅ MCP HTTP服务器启动成功，端口: " + getListeningPort());
            return true;
        } catch (IOException e) {
            Log.e(TAG, "❌ HTTP服务器启动失败: " + e.getMessage(), e);
            return false;
        }
    }
    
    /**
     * 停止HTTP服务器
     */
    public void stopServer() {
        if (isStarted) {
            stop();
            isStarted = false;
            Log.d(TAG, "HTTP服务器已停止");
        }
    }
    
    /**
     * 检查服务器是否正在运行
     */
    public boolean isRunning() {
        return isStarted && isAlive();
    }
    
    @Override
    public Response serve(IHTTPSession session) {
        String uri = session.getUri();
        Method method = session.getMethod();
        
        Log.i(TAG, "=== 收到HTTP请求 ===");
        Log.i(TAG, "请求方法: " + method);
        Log.i(TAG, "请求路径: " + uri);
        Log.i(TAG, "远程地址: " + session.getRemoteIpAddress());
        
        // 打印所有请求头
        Map<String, String> headers = session.getHeaders();
        Log.i(TAG, "请求头信息:");
        for (Map.Entry<String, String> header : headers.entrySet()) {
            Log.i(TAG, "  " + header.getKey() + ": " + header.getValue());
        }
        
        // 打印查询参数
        Map<String, List<String>> parameters = session.getParameters();
        if (!parameters.isEmpty()) {
            Log.i(TAG, "查询参数:");
            for (Map.Entry<String, List<String>> param : parameters.entrySet()) {
                Log.i(TAG, "  " + param.getKey() + ": " + param.getValue());
            }
        }
        
        // 处理CORS预检请求
        if (Method.OPTIONS.equals(method)) {
            Log.i(TAG, "处理CORS预检请求");
            return createCORSResponse("");
        }
        
        // 打印所有非OPTIONS请求
        Log.i(TAG, "请求详情: " + method + " " + uri);
        
        // 对于非POST /mcp请求也要详细记录
        if (!"/mcp".equals(uri) || !Method.POST.equals(method)) {
            Log.w(TAG, "❌ 不支持的请求: " + method + " " + uri);
            Log.w(TAG, "期望的请求: POST /mcp");
            return newFixedLengthResponse(Response.Status.NOT_FOUND, 
                MIME_PLAINTEXT, "MCP服务仅支持POST /mcp，当前请求: " + method + " " + uri);
        }
        
        try {
            // 强制使用UTF-8读取请求体
            String requestBody = getRequestBodyUTF8(session);
            Log.i(TAG, "✅ 请求体长度: " + (requestBody != null ? requestBody.length() : 0));
            Log.i(TAG, "✅ 请求体内容: " + (requestBody != null ? requestBody : "null"));
            
            // 处理JSON-RPC请求
            String responseBody = jsonRpcHandler.handleRequest(requestBody);
            Log.i(TAG, "✅ 响应体长度: " + (responseBody != null ? responseBody.length() : 0));
            Log.i(TAG, "✅ 响应体内容: " + (responseBody != null ? responseBody : "null"));
            
            // 对于通知请求，响应体可能为空，此时返回204 No Content
            if (responseBody == null || responseBody.trim().isEmpty()) {
                Log.i(TAG, "通知请求无需响应内容");
                Response response = newFixedLengthResponse(Response.Status.NO_CONTENT, "application/json", "");
                // 添加CORS头
                response.addHeader("Access-Control-Allow-Origin", "*");
                response.addHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
                response.addHeader("Access-Control-Allow-Headers", "Content-Type, Authorization");
                return response;
            }
            
            return createCORSResponse(responseBody);
            
        } catch (Exception e) {
            Log.e(TAG, "❌ 处理HTTP请求失败", e);
            String errorResponse = jsonRpcHandler.createErrorResponse(null, -32603, 
                "Internal error: " + e.getMessage());
            Log.e(TAG, "❌ 错误响应: " + errorResponse);
            return createCORSResponse(errorResponse);
        } finally {
            Log.i(TAG, "=== HTTP请求处理完成 ===");
        }
    }
    
    /**
     * 创建支持CORS的响应
     */
    private Response createCORSResponse(String content) {
        Response response = newFixedLengthResponse(Response.Status.OK, 
            "application/json; charset=utf-8", content);
        
        // 添加CORS头和编码声明
        response.addHeader("Access-Control-Allow-Origin", "*");
        response.addHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
        response.addHeader("Access-Control-Allow-Headers", "Content-Type, Authorization");
        response.addHeader("Access-Control-Max-Age", "3600");
        response.addHeader("Content-Type", "application/json; charset=utf-8");
        
        return response;
    }
    
    /**
     * 强制使用UTF-8编码读取HTTP请求体
     * 绕过NanoHTTPD的默认编码处理，直接从字节流读取并用UTF-8解码
     */
    private String getRequestBodyUTF8(IHTTPSession session) throws IOException {
        try {
            // 获取Content-Length头
            String contentLengthStr = session.getHeaders().get("content-length");
            if (contentLengthStr == null || contentLengthStr.isEmpty()) {
                Log.d(TAG, "没有Content-Length头，使用fallback方法");
                return getRequestBodyFallback(session);
            }
            
            int contentLength = Integer.parseInt(contentLengthStr);
            Log.d(TAG, "Content-Length: " + contentLength);
            
            if (contentLength <= 0) {
                return "";
            }
            
            // 直接读取原始字节流
            InputStream inputStream = session.getInputStream();
            byte[] buffer = new byte[contentLength];
            
            int totalBytesRead = 0;
            while (totalBytesRead < contentLength) {
                int bytesRead = inputStream.read(buffer, totalBytesRead, contentLength - totalBytesRead);
                if (bytesRead == -1) {
                    break; // EOF
                }
                totalBytesRead += bytesRead;
            }
            
            // 强制使用UTF-8解码
            String result = new String(buffer, 0, totalBytesRead, StandardCharsets.UTF_8);
            Log.d(TAG, "UTF-8强制解码成功，读取字节数: " + totalBytesRead);
            
            return result;
            
        } catch (Exception e) {
            Log.w(TAG, "UTF-8强制解码失败，使用fallback: " + e.getMessage());
            return getRequestBodyFallback(session);
        }
    }
    
    /**
     * Fallback方法：使用NanoHTTPD默认方式读取请求体
     */
    private String getRequestBodyFallback(IHTTPSession session) throws IOException {
        Map<String, String> files = new HashMap<>();
        try {
            session.parseBody(files);
            return files.getOrDefault("postData", "");
        } catch (ResponseException e) {
            Log.e(TAG, "Fallback解析请求体失败: " + e.getMessage());
            throw new IOException("Failed to parse request body", e);
        }
    }
}
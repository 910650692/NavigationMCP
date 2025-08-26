package com.sgm.navi.mcp.coordinator;

import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.Service;
import android.content.Intent;
import android.os.Build;
import android.os.IBinder;
import android.os.RemoteException;
import android.util.Log;

import com.sgm.navi.mcp.IMCPService;
import com.sgm.navi.mcp.IToolCallback;
import com.sgm.navi.mcp.ToolRequest;
import com.sgm.navi.mcp.ToolResponse;

// 新SDK接口
import com.sgm.mcp.IMcpService;
import com.sgm.mcp.IMcpCallback;
import com.sgm.mcp.data.ToolBatchReport;
import com.sgm.mcp.data.ToolInfo;
import com.sgm.mcp.data.ToolResult;

import java.util.List;
import java.util.ArrayList;

/**
 * MCP协调中心服务
 * 负责协调AI智能体和各种工具服务之间的通信
 * 支持HTTP JSON-RPC和Android AIDL双重接口
 */
public class MCPCoordinatorService extends Service {
    private static final String TAG = "MCPCoordinatorService";
    private static final String CHANNEL_ID = "MCP_COORDINATOR_SERVICE";
    private static final int NOTIFICATION_ID = 2001;
    
    private ToolRegistry toolRegistry;
    private RequestRouter requestRouter;
    private HttpServerManager httpServer;
    private JsonRpcHandler jsonRpcHandler;
    
    // 新SDK支持
    private ClientManager clientManager;
    
    @Override
    public void onCreate() {
        super.onCreate();
        
        // 创建前台服务通知
        createNotificationChannel();
        startForeground(NOTIFICATION_ID, createNotification("MCP协调中心启动中..."));
        
        // 初始化核心组件
        toolRegistry = new ToolRegistry();
        requestRouter = new RequestRouter(this, toolRegistry);
        clientManager = new ClientManager();
        
        // 初始化HTTP服务器
        initHttpServer();
        
        Log.d(TAG, "MCP协调中心服务启动，支持AIDL和HTTP JSON-RPC接口");
    }
    
    /**
     * 初始化HTTP服务器
     */
    private void initHttpServer() {
        try {
            jsonRpcHandler = new JsonRpcHandler(mcpBinder, toolRegistry);
            httpServer = new HttpServerManager(jsonRpcHandler);
            
            if (httpServer.startServer()) {
                Log.d(TAG, "✅ HTTP服务器启动成功，外部客户端可通过HTTP访问MCP服务");
                updateNotification("HTTP服务运行中 (端口:8080)");
            } else {
                Log.e(TAG, "❌ HTTP服务器启动失败");
                updateNotification("HTTP服务启动失败");
            }
        } catch (Exception e) {
            Log.e(TAG, "初始化HTTP服务器失败", e);
        }
    }
    
    @Override
    public IBinder onBind(Intent intent) {
        String action = intent.getAction();
        Log.d(TAG, "客户端绑定MCP协调中心服务, action: " + action);
        
        // 根据不同的action返回不同的Binder
        if ("com.sgm.mcp.MCP_SERVICE".equals(action) || 
            "com.sgm.mcpservice.MCP_SERVICE".equals(action)) {
            Log.d(TAG, "返回新SDK兼容的Binder");
            return newSdkBinder;
        } else {
            Log.d(TAG, "返回传统AIDL Binder");
            return mcpBinder;
        }
    }
    
    // 新SDK兼容的Binder实现
    private final IMcpService.Stub newSdkBinder = new IMcpService.Stub() {
        @Override
        public boolean registerMcp(String clientId, IMcpCallback callback) throws RemoteException {
            Log.d(TAG, "新SDK - 注册MCP客户端: " + clientId);
            boolean result = clientManager.registerClient(clientId, callback);
            
            if (result) {
                updateNotification("新SDK客户端已连接: " + clientId);
            }
            
            return result;
        }
        
        @Override
        public boolean unregisterMcp(String clientId) throws RemoteException {
            Log.d(TAG, "新SDK - 注销MCP客户端: " + clientId);
            boolean result = clientManager.unregisterClient(clientId);
            
            if (result) {
                updateNotification("新SDK客户端已断开: " + clientId);
            }
            
            return result;
        }
        
        @Override
        public ToolBatchReport registerTools(String clientId, List<ToolInfo> tools) throws RemoteException {
            Log.d(TAG, "新SDK - 批量注册工具: " + clientId + ", 工具数量: " + tools.size());
            
            long startTime = System.currentTimeMillis();
            ToolBatchReport report = new ToolBatchReport(tools.size());
            
            for (ToolInfo toolInfo : tools) {
                try {
                    // 注册到传统工具注册表（用于HTTP接口兼容）
                    toolRegistry.registerTool(
                        toolInfo.getName(), 
                        toolInfo.getDescription(), 
                        toolInfo.getSchema(), 
                        clientId
                    );
                    
                    // 注册到新SDK客户端管理器
                    clientManager.registerToolForClient(clientId, toolInfo);
                    
                    report.addSuccess(toolInfo.getName());
                    Log.d(TAG, "工具注册成功: " + toolInfo.getName());
                    
                } catch (Exception e) {
                    report.addFailure(toolInfo.getName(), e.getMessage());
                    Log.e(TAG, "工具注册失败: " + toolInfo.getName(), e);
                }
            }
            
            long executionTime = System.currentTimeMillis() - startTime;
            report.setExecutionTimeMs(executionTime);
            
            Log.d(TAG, "批量工具注册完成: 成功" + report.getSuccessCount() + 
                      ", 失败" + report.getFailureCount() + ", 耗时" + executionTime + "ms");
                      
            updateNotification("已注册" + clientManager.getTotalToolCount() + "个工具");
            
            return report;
        }
        
        @Override
        public ToolBatchReport unregisterTools(String clientId, List<String> toolNames) throws RemoteException {
            Log.d(TAG, "新SDK - 批量注销工具: " + clientId + ", 工具数量: " + toolNames.size());
            
            long startTime = System.currentTimeMillis();
            ToolBatchReport report = new ToolBatchReport(toolNames.size());
            
            for (String toolName : toolNames) {
                try {
                    // 从传统工具注册表注销
                    toolRegistry.unregisterTool(toolName);
                    
                    // 从新SDK客户端管理器注销
                    clientManager.unregisterToolForClient(clientId, toolName);
                    
                    report.addSuccess(toolName);
                    Log.d(TAG, "工具注销成功: " + toolName);
                    
                } catch (Exception e) {
                    report.addFailure(toolName, e.getMessage());
                    Log.e(TAG, "工具注销失败: " + toolName, e);
                }
            }
            
            long executionTime = System.currentTimeMillis() - startTime;
            report.setExecutionTimeMs(executionTime);
            
            Log.d(TAG, "批量工具注销完成: 成功" + report.getSuccessCount() + 
                      ", 失败" + report.getFailureCount() + ", 耗时" + executionTime + "ms");
            
            return report;
        }
        
        @Override
        public List<String> getToolNames(String clientId) throws RemoteException {
            List<String> tools = clientManager.getClientTools(clientId);
            Log.d(TAG, "新SDK - 获取客户端工具列表: " + clientId + ", 数量: " + tools.size());
            return tools;
        }
        
        @Override
        public List<String> getAllToolNames() throws RemoteException {
            List<String> allTools = clientManager.getAllToolNames();
            Log.d(TAG, "新SDK - 获取所有工具列表, 数量: " + allTools.size());
            return allTools;
        }
    };
    
    // 传统AIDL Binder实现（保持兼容性）
    private final IMCPService.Stub mcpBinder = new IMCPService.Stub() {
        @Override
        public void registerTool(String name, String description, String schema, String packageName) 
                throws RemoteException {
            Log.d(TAG, "注册工具: " + name + " from " + packageName);
            toolRegistry.registerTool(name, description, schema, packageName);
        }
        
        @Override
        public void callTool(ToolRequest request, IToolCallback callback) throws RemoteException {
            String toolName = request.getToolName();
            Log.d(TAG, "调用工具: " + toolName + ", 参数: " + request.getParameters());
            
            // 首先尝试通过新SDK客户端管理器调用
            android.os.Bundle params = ClientManager.jsonStringToBundle(request.getParameters());
            ToolResult newSdkResult = clientManager.executeToolCall(toolName, params);
            
            if (newSdkResult != null && newSdkResult.isSuccess()) {
                // 新SDK成功处理，转换结果格式
                try {
                    ToolResponse response = new ToolResponse(
                        newSdkResult.getToolName(),
                        newSdkResult.getResult() != null ? newSdkResult.getResult() : newSdkResult.getErrorMessage(),
                        newSdkResult.isSuccess()
                    );
                    callback.onToolResult(response);
                    Log.d(TAG, "工具调用成功(新SDK): " + toolName);
                } catch (RemoteException e) {
                    Log.e(TAG, "返回结果失败", e);
                }
            } else {
                // 回退到传统路由
                requestRouter.routeToolCall(request, new RequestRouter.ToolCallCallback() {
                    @Override
                    public void onSuccess(ToolResponse response) {
                        try {
                            callback.onToolResult(response);
                            Log.d(TAG, "工具调用成功(传统): " + toolName);
                        } catch (RemoteException e) {
                            Log.e(TAG, "返回结果失败", e);
                        }
                    }
                    
                    @Override
                    public void onError(String error) {
                        try {
                            callback.onToolError(error);
                            Log.e(TAG, "工具调用失败: " + toolName + ", 错误: " + error);
                        } catch (RemoteException e) {
                            Log.e(TAG, "返回错误失败", e);
                        }
                    }
                });
            }
        }
        
        @Override
        public String[] getRegisteredTools() throws RemoteException {
            // 合并传统工具和新SDK工具
            String[] legacyTools = toolRegistry.getAllToolNames();
            List<String> newSdkTools = clientManager.getAllToolNames();
            
            List<String> allTools = new ArrayList<>();
            for (String tool : legacyTools) {
                allTools.add(tool);
            }
            for (String tool : newSdkTools) {
                if (!allTools.contains(tool)) {
                    allTools.add(tool);
                }
            }
            
            String[] result = allTools.toArray(new String[0]);
            Log.d(TAG, "获取已注册工具列表，共 " + result.length + " 个工具 (传统:" + 
                      legacyTools.length + ", 新SDK:" + newSdkTools.size() + ")");
            return result;
        }
    };
    
    @Override
    public void onDestroy() {
        super.onDestroy();
        
        // 停止HTTP服务器
        if (httpServer != null) {
            httpServer.stopServer();
            Log.d(TAG, "HTTP服务器已停止");
        }
        
        Log.d(TAG, "MCP协调中心服务停止");
    }
    
    /**
     * 创建通知渠道
     */
    private void createNotificationChannel() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            NotificationChannel channel = new NotificationChannel(
                CHANNEL_ID,
                "MCP协调中心服务",
                NotificationManager.IMPORTANCE_LOW
            );
            channel.setDescription("MCP协调中心服务，提供HTTP和AIDL接口");
            
            NotificationManager manager = getSystemService(NotificationManager.class);
            manager.createNotificationChannel(channel);
        }
    }
    
    /**
     * 创建前台服务通知
     */
    private Notification createNotification(String content) {
        return new Notification.Builder(this, CHANNEL_ID)
            .setContentTitle("MCP协调中心")
            .setContentText(content)
            .setSmallIcon(android.R.drawable.ic_menu_manage)
            .build();
    }
    
    /**
     * 更新通知内容
     */
    private void updateNotification(String content) {
        Notification notification = createNotification(content);
        NotificationManager manager = getSystemService(NotificationManager.class);
        manager.notify(NOTIFICATION_ID, notification);
    }
}
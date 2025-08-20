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
    
    @Override
    public void onCreate() {
        super.onCreate();
        
        // 创建前台服务通知
        createNotificationChannel();
        startForeground(NOTIFICATION_ID, createNotification("MCP协调中心启动中..."));
        
        // 初始化核心组件
        toolRegistry = new ToolRegistry();
        requestRouter = new RequestRouter(this, toolRegistry);
        
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
        Log.d(TAG, "客户端绑定MCP协调中心服务");
        return mcpBinder;
    }
    
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
            
            requestRouter.routeToolCall(request, new RequestRouter.ToolCallCallback() {
                @Override
                public void onSuccess(ToolResponse response) {
                    try {
                        callback.onToolResult(response);
                        Log.d(TAG, "工具调用成功: " + toolName);
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
        
        @Override
        public String[] getRegisteredTools() throws RemoteException {
            String[] tools = toolRegistry.getAllToolNames();
            Log.d(TAG, "获取已注册工具列表，共 " + tools.length + " 个工具");
            return tools;
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
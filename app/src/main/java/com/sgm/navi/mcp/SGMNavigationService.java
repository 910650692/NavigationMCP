package com.sgm.navi.mcp;

import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.Service;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.os.Build;
import android.os.IBinder;
import android.os.RemoteException;
import android.util.Log;

import com.sgm.navi.mcp.core.Mcp;
import com.sgm.navi.mcp.tools.SGMNavigationTools;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.hmi.R;

/**
 * SGM导航MCP工具服务
 * 作为MCP客户端：连接MCP协调中心，注册导航工具，响应工具调用
 */
public class SGMNavigationService extends Service {
    private static final String TAG = "SGMNavigationService";
    private static final String CHANNEL_ID = "SGM_MCP_SERVICE";
    private static final int NOTIFICATION_ID = 1001;
    
    private IMCPService mcpCoordinatorService;
    private boolean isConnectedToCoordinator = false;

    @Override
    public void onCreate() {
        super.onCreate();
        Log.d(TAG, "SGM导航MCP服务启动");
        
        // 创建前台服务通知
        createNotificationChannel();
        startForeground(NOTIFICATION_ID, createNotification());
        
        // 连接到MCP协调中心
        connectToMCPCoordinator();
    }
    
    /**
     * 连接到MCP协调中心
     */
    private void connectToMCPCoordinator() {
        try {
            Intent intent = new Intent();
            intent.setComponent(new ComponentName("com.sgm.navi.hmi", 
                "com.sgm.navi.mcp.coordinator.MCPCoordinatorService"));
            
            boolean result = bindService(intent, mcpCoordinatorConnection, Context.BIND_AUTO_CREATE);
            Log.d(TAG, "连接MCP协调中心: " + (result ? "成功" : "失败"));
            
        } catch (Exception e) {
            Log.e(TAG, "连接MCP协调中心失败: " + e.getMessage());
        }
    }
    
    private final ServiceConnection mcpCoordinatorConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName name, IBinder service) {
            mcpCoordinatorService = IMCPService.Stub.asInterface(service);
            isConnectedToCoordinator = true;
            Log.d(TAG, "✅ 已连接到MCP协调中心");
            
            updateNotification("已连接，正在注册工具...");
            
            // 注册导航工具到协调中心
            registerNavigationTools();
        }

        @Override
        public void onServiceDisconnected(ComponentName name) {
            mcpCoordinatorService = null;
            isConnectedToCoordinator = false;
            Log.d(TAG, "❌ 与MCP协调中心连接断开");
            
            updateNotification("连接断开，等待重连...");
        }
    };
    
    /**
     * 注册导航工具到MCP协调中心
     */
    private void registerNavigationTools() {
        ThreadManager.getInstance().execute(() -> {
            if (mcpCoordinatorService != null && isConnectedToCoordinator) {
                try {
                    // 1. 设置MCP协调中心服务连接
                    Mcp.setMCPCoordinatorService(mcpCoordinatorService);
                    
                    // 2. 创建SGM导航工具实例并使用注解驱动注册
                    SGMNavigationTools navigationTools = new SGMNavigationTools();
                    Mcp.registerTool(navigationTools);
                    
                    int registeredCount = Mcp.getRegisteredToolCount();
                    String[] toolNames = Mcp.getRegisteredToolNames();
                    
                    Log.d(TAG, 
                        String.format("✅ 导航工具注册完成，共注册 %d 个工具: %s", 
                        registeredCount, String.join(", ", toolNames)));
                        
                    Log.d(TAG, "🚀 SGM导航工具已就绪，等待AI智能体调用");
                    
                    updateNotification("工具已注册 (" + registeredCount + "个)，服务就绪");
                } catch (Exception e) {
                    Log.e(TAG, "❌ 导航工具注册失败: " + e.getMessage());
                }
            }
        });
    }

    @Override
    public IBinder onBind(Intent intent) {
        Log.d(TAG, "客户端绑定SGM导航服务");
        return navigationBinder;
    }

    // 注意：这里使用的AIDL接口来自mcp-coordinator模块
    private final ISGMNavigationService.Stub navigationBinder = new ISGMNavigationService.Stub() {
        @Override
        public ToolResponse callTool(ToolRequest request) throws RemoteException {
            String toolName = request.getToolName();
            String parameters = request.getParameters();
            
            Log.d(TAG, "收到工具调用请求: " + toolName);
            
            try {
                // 使用MCP核心引擎调用工具
                String result = Mcp.invokeTool(toolName, parameters);
                
                return new ToolResponse(toolName, result, true);
                
            } catch (Exception e) {
                Log.e(TAG, "工具调用失败: " + toolName, e);
                return new ToolResponse(toolName, "工具调用失败: " + e.getMessage(), false);
            }
        }
    };

    /**
     * 创建通知渠道
     */
    private void createNotificationChannel() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            NotificationChannel channel = new NotificationChannel(
                CHANNEL_ID,
                "SGM MCP服务",
                NotificationManager.IMPORTANCE_LOW
            );
            channel.setDescription("SGM导航MCP工具服务");
            
            NotificationManager manager = getSystemService(NotificationManager.class);
            manager.createNotificationChannel(channel);
        }
    }
    
    /**
     * 创建前台服务通知
     */
    private Notification createNotification() {
        return new Notification.Builder(this, CHANNEL_ID)
            .setContentTitle("SGM MCP服务")
            .setContentText("正在连接MCP协调中心...")
            .setSmallIcon(R.drawable.ic_launcher_foreground)
            .build();
    }
    
    /**
     * 更新通知内容
     */
    private void updateNotification(String content) {
        Notification notification = new Notification.Builder(this, CHANNEL_ID)
            .setContentTitle("SGM MCP服务")
            .setContentText(content)
            .setSmallIcon(R.drawable.ic_launcher_foreground)
            .build();
            
        NotificationManager manager = getSystemService(NotificationManager.class);
        manager.notify(NOTIFICATION_ID, notification);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        
        // 断开与MCP协调中心的连接
        if (isConnectedToCoordinator) {
            unbindService(mcpCoordinatorConnection);
        }
        
        Log.d(TAG, "SGM导航MCP服务停止");
    }
}
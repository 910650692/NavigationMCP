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
import android.os.Bundle;
import android.os.IBinder;
import android.os.RemoteException;
import android.util.Log;

// 原始MCP Core引用
import com.sgm.navi.mcp.core.Mcp;

// Tools类（需要修改注解）
import com.sgm.navi.mcp.tools.LocationTools;
import com.sgm.navi.mcp.tools.SearchTools;
import com.sgm.navi.mcp.tools.NavigationTools;
import com.sgm.navi.mcp.tools.FavoriteTools;
import com.sgm.navi.mcp.tools.SettingTools;

// AIDL interfaces from mcp-coordinator
import com.sgm.navi.mcp.ISGMNavigationService;
import com.sgm.navi.mcp.ToolRequest;
import com.sgm.navi.mcp.ToolResponse;
import com.sgm.navi.mcp.IMCPService;

import java.util.Arrays;
import java.util.List;
import androidx.annotation.NonNull;
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
    
    // 旧版本AIDL连接（保留作为备用）
    // private IMCPService mcpCoordinatorService;
    // private boolean isConnectedToCoordinator = false;
    

    @Override
    public void onCreate() {
        super.onCreate();
        Log.d(TAG, "SGM导航MCP服务启动");
        
        // 创建前台服务通知
        createNotificationChannel();
        startForeground(NOTIFICATION_ID, createNotification());
        
        // 先连接到coordinator，然后再初始化MCP core
        initLegacyToolRegistration();
    }
    
    /**
     * 初始化原始MCP core
     */
    private void initMcp() {
        try {
            Log.d(TAG, "🚀 初始化原始MCP Core");
            updateNotification("正在初始化MCP Core...");
            
            // 创建工具实例并注册到MCP core
            LocationTools locationTools = new LocationTools();
            SearchTools searchTools = new SearchTools();
            NavigationTools navigationTools = new NavigationTools();
            FavoriteTools favoriteTools = new FavoriteTools();
            SettingTools settingTools = new SettingTools();
            
            // 注册所有工具到原始MCP core
            Mcp.registerTool(locationTools);
            Mcp.registerTool(searchTools);
            Mcp.registerTool(navigationTools);
            Mcp.registerTool(favoriteTools);
            Mcp.registerTool(settingTools);
            
            Log.d(TAG, "✅ MCP Core初始化完成，已注册 " + Mcp.getRegisteredToolCount() + " 个工具");
            updateNotification("MCP服务就绪 - " + Mcp.getRegisteredToolCount() + " 个工具");
            
        } catch (Exception e) {
            Log.e(TAG, "❌ MCP Core初始化失败", e);
            updateNotification("MCP初始化失败: " + e.getMessage());
        }
    }
    
    
    /**
     * 使用传统AIDL方式注册工具到coordinator（备用方案）
     */
    private void initLegacyToolRegistration() {
        ThreadManager.getInstance().execute(() -> {
            try {
                Log.d(TAG, "🔄 启动传统AIDL工具注册...");
                
                // 连接到MCP协调中心 - 使用传统AIDL接口
                Intent intent = new Intent();
                ComponentName componentName = new ComponentName(
                    "com.sgm.navi.hmi", 
                    "com.sgm.navi.mcp.coordinator.MCPCoordinatorService"
                );
                intent.setComponent(componentName);
                // 不设置action，让coordinator返回默认的传统AIDL接口
                
                boolean bindResult = bindService(intent, legacyMcpConnection, Context.BIND_AUTO_CREATE);
                Log.d(TAG, bindResult ? "✅ 传统AIDL连接请求成功" : "❌ 传统AIDL连接请求失败");
                
            } catch (Exception e) {
                Log.e(TAG, "❌ 传统AIDL工具注册失败", e);
            }
        });
    }
    
    // 传统AIDL连接
    private IMCPService legacyMcpService;
    private boolean isLegacyConnected = false;
    
    private final ServiceConnection legacyMcpConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName name, IBinder service) {
            legacyMcpService = IMCPService.Stub.asInterface(service);
            isLegacyConnected = true;
            
            Log.d(TAG, "✅ 传统AIDL - 已连接到MCP协调中心，开始初始化MCP Core...");
            
            // 配置MCP Core并注册工具
            registerLegacyTools();
            
            // 现在初始化MCP Core（此时coordinator服务已连接）
            initMcp();
        }
        
        @Override
        public void onServiceDisconnected(ComponentName name) {
            legacyMcpService = null;
            isLegacyConnected = false;
            Log.w(TAG, "⚠️ 传统AIDL - MCP协调中心连接断开");
        }
    };
    
    /**
     * 使用传统AIDL方式注册工具（从ToolProvider自动提取信息）
     */
    private void registerLegacyTools() {
        if (legacyMcpService == null) {
            Log.e(TAG, "❌ 传统AIDL - MCP服务未连接");
            return;
        }
        
        Log.d(TAG, "📝 传统AIDL - 配置MCP Core使用coordinator服务");

        try {
            // 将coordinator服务设置到MCP Core
            Mcp.setMCPCoordinatorService(legacyMcpService);
            
            // 现在MCP Core可以自动注册工具到coordinator
            // initMcp()中的Mcp.registerTool()调用现在会工作
            
            // 获取已注册的工具数量
            String[] toolNames = Mcp.getRegisteredToolNames();
            Log.d(TAG, "✅ MCP Core已连接coordinator，共有 " + toolNames.length + " 个工具");
            
            for (String toolName : toolNames) {
                Log.d(TAG, "   🔧 工具: " + toolName);
            }
            
            updateNotification("MCP Core已连接coordinator - " + toolNames.length + " 个工具");

        } catch (Exception e) {
            Log.e(TAG, "❌ 配置MCP Core失败", e);
            updateNotification("MCP Core配置失败: " + e.getMessage());
        }

    }
    
    
    

    @Override
    public IBinder onBind(Intent intent) {
        Log.d(TAG, "客户端绑定SGM导航服务");
        return navigationBinder;  // 暂时保留原有的AIDL Binder接口
    }

    // 注意：这里使用的AIDL接口来自mcp-coordinator模块
    private final ISGMNavigationService.Stub navigationBinder = new ISGMNavigationService.Stub() {
        @Override
        public ToolResponse callTool(ToolRequest request) throws RemoteException {
            String toolName = request.getToolName();
            String parameters = request.getParameters();
            
            Log.d(TAG, "📞 收到工具调用请求: " + toolName + " (参数: " + parameters + ")");
            
            try {
                // 使用原始MCP core调用工具
                String result = Mcp.invokeTool(toolName, parameters);
                
                Log.d(TAG, "✅ 工具调用成功: " + toolName);
                return new ToolResponse(toolName, result, true);
                
            } catch (Exception e) {
                Log.e(TAG, "❌ 工具调用失败: " + toolName, e);
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
            .setContentText("正在初始化新SDK连接...")
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
        
        Log.d(TAG, "🛑 SGM导航MCP服务正在停止...");
        
        // 断开传统AIDL连接
        if (isLegacyConnected && legacyMcpService != null) {
            try {
                unbindService(legacyMcpConnection);
                Log.d(TAG, "✅ 传统AIDL连接已断开");
            } catch (Exception e) {
                Log.w(TAG, "⚠️ 断开传统AIDL连接时出现异常", e);
            } finally {
                legacyMcpService = null;
                isLegacyConnected = false;
            }
        }
        
        Log.d(TAG, "🏁 SGM导航MCP服务已完全停止");
    }
}
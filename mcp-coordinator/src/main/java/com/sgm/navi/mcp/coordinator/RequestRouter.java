package com.sgm.navi.mcp.coordinator;

import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.os.IBinder;
import android.os.RemoteException;
import android.util.Log;

import com.sgm.navi.mcp.ISGMNavigationService;
import com.sgm.navi.mcp.ToolRequest;
import com.sgm.navi.mcp.ToolResponse;

import java.util.HashMap;
import java.util.Map;

/**
 * MCP请求路由器
 * 负责将工具调用请求路由到正确的服务
 */
public class RequestRouter {
    private static final String TAG = "RequestRouter";
    
    private final Context context;
    private final ToolRegistry toolRegistry;
    private final Map<String, ISGMNavigationService> serviceConnections = new HashMap<>();
    
    public RequestRouter(Context context, ToolRegistry toolRegistry) {
        this.context = context;
        this.toolRegistry = toolRegistry;
    }
    
    /**
     * 路由工具调用请求
     */
    public void routeToolCall(ToolRequest request, ToolCallCallback callback) {
        String toolName = request.getToolName();
        
        ToolRegistry.RegisteredTool tool = toolRegistry.getTool(toolName);
        if (tool == null) {
            callback.onError("工具未找到: " + toolName);
            return;
        }
        
        // 获取或创建服务连接
        getServiceConnection(tool.packageName, new ServiceConnectionCallback() {
            @Override
            public void onServiceConnected(ISGMNavigationService service) {
                try {
                    ToolResponse response = service.callTool(request);
                    callback.onSuccess(response);
                } catch (RemoteException e) {
                    Log.e(TAG, "调用远程服务失败", e);
                    callback.onError("远程调用失败: " + e.getMessage());
                }
            }
            
            @Override
            public void onServiceError(String error) {
                callback.onError(error);
            }
        });
    }
    
    /**
     * 获取服务连接
     */
    private void getServiceConnection(String packageName, ServiceConnectionCallback callback) {
        // 检查是否已有连接
        ISGMNavigationService existingService = serviceConnections.get(packageName);
        if (existingService != null) {
            callback.onServiceConnected(existingService);
            return;
        }
        
        // 创建新连接
        Intent intent = new Intent();
        intent.setComponent(new ComponentName(packageName, 
            "com.sgm.navi.mcp.SGMNavigationService"));
        
        ServiceConnection connection = new ServiceConnection() {
            @Override
            public void onServiceConnected(ComponentName name, IBinder service) {
                ISGMNavigationService navigationService = ISGMNavigationService.Stub.asInterface(service);
                serviceConnections.put(packageName, navigationService);
                callback.onServiceConnected(navigationService);
                Log.d(TAG, "导航服务连接成功: " + packageName);
            }
            
            @Override
            public void onServiceDisconnected(ComponentName name) {
                serviceConnections.remove(packageName);
                Log.d(TAG, "导航服务连接断开: " + packageName);
            }
        };
        
        boolean success = context.bindService(intent, connection, Context.BIND_AUTO_CREATE);
        if (!success) {
            callback.onServiceError("无法连接到导航服务: " + packageName);
        }
    }
    
    /**
     * 工具调用回调接口
     */
    public interface ToolCallCallback {
        void onSuccess(ToolResponse response);
        void onError(String error);
    }
    
    /**
     * 服务连接回调接口
     */
    private interface ServiceConnectionCallback {
        void onServiceConnected(ISGMNavigationService service);
        void onServiceError(String error);
    }
}
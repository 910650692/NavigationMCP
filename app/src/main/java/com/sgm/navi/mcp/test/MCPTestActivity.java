package com.sgm.navi.mcp.test;

import android.app.Activity;
import android.content.ComponentName;
import android.content.Intent;
import android.content.ServiceConnection;
import android.os.Bundle;
import android.os.IBinder;
import android.os.RemoteException;
import android.text.method.ScrollingMovementMethod;
import android.util.Log;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ScrollView;
import android.widget.TextView;

import com.sgm.navi.hmi.R;
import com.sgm.navi.mcp.IMCPService;
import com.sgm.navi.mcp.ToolRequest;
import com.sgm.navi.mcp.ToolResponse;
import com.sgm.navi.mcp.IToolCallback;

/**
 * MCP测试Activity
 * 用于测试MCP工具调用和查看JSON请求响应
 */
public class MCPTestActivity extends Activity {
    private static final String TAG = "MCPTestActivity";

    private TextView logTextView;
    private EditText toolNameEdit;
    private EditText parametersEdit;
    private Button testButton;
    private Button refreshToolsButton;
    private Button clearButton;
    private TextView connectionStatusText;
    private ScrollView logScrollView;

    private IMCPService mcpService;
    private boolean isServiceConnected = false;
    private String[] availableTools;

    private final ServiceConnection serviceConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName name, IBinder service) {
            mcpService = IMCPService.Stub.asInterface(service);
            isServiceConnected = true;
            appendLog("✅ MCP协调中心连接成功");
            Log.d(TAG, "MCP服务连接成功");
            
            updateConnectionStatus(true, 0);
            
            // 自动查询可用工具列表
            queryAvailableTools();
        }

        @Override
        public void onServiceDisconnected(ComponentName name) {
            mcpService = null;
            isServiceConnected = false;
            availableTools = null;
            appendLog("❌ MCP协调中心连接断开");
            Log.d(TAG, "MCP服务连接断开");
            
            updateConnectionStatus(false, 0);
        }
    };

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_mcp_test);

        initViews();
        bindMCPService();
    }

    private void initViews() {
        logTextView = findViewById(R.id.logTextView);
        toolNameEdit = findViewById(R.id.toolNameEdit);
        parametersEdit = findViewById(R.id.parametersEdit);
        testButton = findViewById(R.id.testButton);
        refreshToolsButton = findViewById(R.id.refreshToolsButton);
        clearButton = findViewById(R.id.clearButton);
        connectionStatusText = findViewById(R.id.connectionStatusText);
        logScrollView = findViewById(R.id.logScrollView);

        logTextView.setMovementMethod(new ScrollingMovementMethod());

        testButton.setOnClickListener(v -> testToolCall());
        refreshToolsButton.setOnClickListener(v -> queryAvailableTools());
        clearButton.setOnClickListener(v -> clearLog());

        // 设置默认测试值
        toolNameEdit.setText("get_current_location");
        parametersEdit.setText("{}");

        updateConnectionStatus(false, 0);
        appendLog("📱 MCP测试界面已启动");
    }

    private void bindMCPService() {
        try {
            Intent intent = new Intent();
            intent.setComponent(new ComponentName("com.sgm.navi.hmi", 
                "com.sgm.navi.mcp.coordinator.MCPCoordinatorService"));
            boolean result = bindService(intent, serviceConnection, BIND_AUTO_CREATE);
            appendLog("🔗 尝试连接MCP协调中心: " + (result ? "请求发送" : "绑定失败"));
        } catch (Exception e) {
            appendLog("❌ MCP服务绑定异常: " + e.getMessage());
            Log.e(TAG, "绑定MCP服务失败", e);
        }
    }

    /**
     * 查询可用工具列表
     */
    private void queryAvailableTools() {
        if (!isServiceConnected) {
            appendLog("❌ MCP服务未连接，无法查询工具列表");
            return;
        }

        try {
            appendLog("🔍 正在查询可用工具列表...");
            availableTools = mcpService.getRegisteredTools();
            
            if (availableTools != null && availableTools.length > 0) {
                appendLog("✅ 发现 " + availableTools.length + " 个已注册工具:");
                for (String tool : availableTools) {
                    appendLog("  🔧 " + tool);
                }
                appendLog("🎯 SGMNavigationService工具注册状态: " + 
                    (availableTools.length > 0 ? "正常" : "未发现工具"));
                    
                updateConnectionStatus(true, availableTools.length);
            } else {
                appendLog("⚠️  未发现任何已注册工具");
                appendLog("💡 请检查SGMNavigationService是否正常启动并注册工具");
                
                updateConnectionStatus(true, 0);
            }
            
        } catch (RemoteException e) {
            appendLog("❌ 查询工具列表失败: " + e.getMessage());
            Log.e(TAG, "查询工具列表失败", e);
        }
    }
    
    /**
     * 更新连接状态显示
     */
    private void updateConnectionStatus(boolean connected, int toolCount) {
        runOnUiThread(() -> {
            if (connected) {
                if (toolCount > 0) {
                    connectionStatusText.setText("🟢 MCP协调中心: 已连接 | 工具: " + toolCount + " 个");
                } else {
                    connectionStatusText.setText("🟡 MCP协调中心: 已连接 | 工具: 等待注册");
                }
            } else {
                connectionStatusText.setText("🔴 MCP协调中心: 未连接");
            }
        });
    }

    private void testToolCall() {
        if (!isServiceConnected) {
            appendLog("❌ MCP服务未连接，无法执行测试");
            appendLog("💡 请等待连接建立或重启应用");
            return;
        }

        String toolName = toolNameEdit.getText().toString().trim();
        String parameters = parametersEdit.getText().toString().trim();

        if (toolName.isEmpty()) {
            appendLog("❌ 请输入工具名称");
            return;
        }

        // 验证工具是否已注册
        if (availableTools == null || availableTools.length == 0) {
            appendLog("⚠️  未发现已注册工具，尝试重新查询...");
            queryAvailableTools();
            return;
        }

        boolean toolExists = false;
        for (String tool : availableTools) {
            if (tool.equals(toolName)) {
                toolExists = true;
                break;
            }
        }

        if (!toolExists) {
            appendLog("⚠️  工具 '" + toolName + "' 未在已注册工具中找到");
            appendLog("📋 可用工具: " + String.join(", ", availableTools));
            appendLog("💡 建议使用已注册的工具名称");
        }

        appendLog("🚀 开始测试工具调用...");
        appendLog("工具名称: " + toolName);
        appendLog("参数: " + parameters);

        try {
            ToolRequest request = new ToolRequest(toolName, parameters);
            
            // 记录JSON请求日志
            logJsonRequest(toolName, request);

            mcpService.callTool(request, new IToolCallback.Stub() {
                @Override
                public void onToolResult(ToolResponse response) throws RemoteException {
                    runOnUiThread(() -> {
                        appendLog("✅ 工具调用完成");
                        logJsonResponse(response);
                        appendLog("结果: " + response.getResult());
                        appendLog("---");
                    });
                }

                @Override
                public void onToolError(String error) throws RemoteException {
                    runOnUiThread(() -> {
                        appendLog("❌ 工具调用失败: " + error);
                        logJsonError(error);
                        appendLog("---");
                    });
                }
            });

        } catch (RemoteException e) {
            appendLog("❌ 远程调用异常: " + e.getMessage());
            Log.e(TAG, "工具调用失败", e);
        }
    }

    /**
     * 记录JSON请求日志
     */
    private void logJsonRequest(String toolName, ToolRequest request) {
        String jsonRequest = String.format(
            "{\n" +
            "  \"jsonrpc\": \"2.0\",\n" +
            "  \"method\": \"tools/call\",\n" +
            "  \"params\": {\n" +
            "    \"name\": \"%s\",\n" +
            "    \"arguments\": %s\n" +
            "  },\n" +
            "  \"id\": \"req_%d\"\n" +
            "}",
            request.getToolName(),
            request.getParameters(),
            System.currentTimeMillis()
        );
        appendLog("📤 发送JSON请求:");
        appendLog("```json\n" + jsonRequest + "\n```");
    }

    /**
     * 记录JSON响应日志
     */
    private void logJsonResponse(ToolResponse response) {
        String jsonResponse = String.format(
            "{\n" +
            "  \"jsonrpc\": \"2.0\",\n" +
            "  \"result\": %s,\n" +
            "  \"id\": \"req_%d\"\n" +
            "}",
            response.getResult(),
            System.currentTimeMillis()
        );
        appendLog("📥 收到JSON响应:");
        appendLog("```json\n" + jsonResponse + "\n```");
    }

    /**
     * 记录JSON错误日志
     */
    private void logJsonError(String error) {
        String jsonError = String.format(
            "{\n" +
            "  \"jsonrpc\": \"2.0\",\n" +
            "  \"error\": {\n" +
            "    \"code\": -32603,\n" +
            "    \"message\": \"Internal error\",\n" +
            "    \"data\": \"%s\"\n" +
            "  },\n" +
            "  \"id\": \"req_%d\"\n" +
            "}",
            error,
            System.currentTimeMillis()
        );
        appendLog("📥 收到JSON错误:");
        appendLog("```json\n" + jsonError + "\n```");
    }

    private void appendLog(String message) {
        runOnUiThread(() -> {
            String timestamp = java.text.DateFormat.getTimeInstance().format(new java.util.Date());
            String logMessage = "[" + timestamp + "] " + message + "\n";
            logTextView.append(logMessage);
            
            // 自动滚动到底部
            logScrollView.post(() -> logScrollView.fullScroll(View.FOCUS_DOWN));
        });
    }

    private void clearLog() {
        logTextView.setText("");
        appendLog("📱 日志已清空");
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        if (isServiceConnected) {
            unbindService(serviceConnection);
        }
    }
}
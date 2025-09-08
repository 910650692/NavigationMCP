package com.sgm.navi;

import android.app.Activity;
import android.app.Application;
import android.content.Intent;
import android.content.res.Configuration;

import androidx.annotation.NonNull;

import com.alibaba.android.arouter.launcher.ARouter;
import com.android.utils.log.Logger;
import com.android.utils.process.ProcessManager;
import com.android.utils.process.ProcessStatus;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.flavor.CarModelsFeature;
import com.sgm.navi.hmi.BuildConfig;
import com.sgm.navi.hmi.map.MapActivity;
import com.sgm.navi.patacnetlib.PatacNetClient;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.MapDefaultFinalTag;

// 新MCP SDK
import com.sgm.mcp.core.Mcp;
import com.sgm.mcp.service.IServerLifecycle;
import com.sgm.mcp.connection.ConnectionState;
import com.sgm.mcp.client.McpClient;
import com.sgm.navi.mcp.tools.LocationTools;
import com.sgm.navi.mcp.tools.SearchTools;
import com.sgm.navi.mcp.tools.NavigationTools;
import com.sgm.navi.mcp.tools.FavoriteTools;
import com.sgm.navi.mcp.tools.SettingTools;
import java.util.Arrays;
import com.sgm.navi.service.StartService;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.mcp.tools.BaseToolHelper;
import com.sgm.navi.ui.BaseApplication;

import java.util.ArrayList;
import java.util.List;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public class NaviApplication extends BaseApplication implements Application.ActivityLifecycleCallbacks {

    private final List<OnConfigurationChangedListener> configChangedListenerList = new ArrayList<>();

    public interface OnConfigurationChangedListener {
        void onConfigurationChanged(@NonNull Configuration newConfig);
    }

    public void addOnConfigurationChangedListener(OnConfigurationChangedListener listener) {
        if (listener != null && !configChangedListenerList.contains(listener)) {
            configChangedListenerList.add(listener);
        }
    }

    public void removeOnConfigurationChangedListener(OnConfigurationChangedListener listener) {
        configChangedListenerList.remove(listener);
    }

    @Override
    public void onCreate() {
        super.onCreate();
        Logger.setDefaultTag(MapDefaultFinalTag.DEFAULT_TAG);
        initARouter();
        initComponent();
        initMcpSdk();
    }

    @Override
    public void onTerminate() {
        super.onTerminate();
        CarModelsFeature.getInstance().unRegisterBroadcast();
        StartService.getInstance().unSdkInit();
    }

    @Override
    public void onConfigurationChanged(@NonNull Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
        if (!configChangedListenerList.isEmpty()) {
            if (Logger.openLog) {
                Logger.d(TAG, "onConfigurationChanged---configChangedListenerList_size:"
                        + configChangedListenerList.size());
            }
            for (OnConfigurationChangedListener listener : configChangedListenerList) {
                listener.onConfigurationChanged(newConfig);
            }
        } else {
            Logger.w(TAG, "onConfigurationChanged未设置监听器");
        }
    }

    @Override
    protected void updateProgressStatus(@NonNull Activity activity, int processStatus) {
        if (activity instanceof MapActivity) {
            ProcessManager.updateProcessStatus(processStatus);
            switch (processStatus) {
                case ProcessStatus.AppRunStatus.RESUMED ->
                        ProcessManager.updateIsAppInForeground(true);
                case ProcessStatus.AppRunStatus.PAUSED ->
                        ProcessManager.updateIsAppInForeground(false);
            }
        }
    }

    /**
     * 初始化路由
     */
    private void initARouter() {
        if (BuildConfig.DEBUG) {
            ARouter.openLog();
            ARouter.openDebug();
        }
        ARouter.init(this);
    }

    private void initComponent() {
        AppCache.getInstance().setMApplication(this);
        AppCache.getInstance().setMContext(getApplicationContext());
        registerActivityLifecycleCallbacks(NaviApplication.this);
        ThreadManager.getInstance().execute(() -> {
            try {
                PatacNetClient.getInstance().init(this); // 初始化网络适配器
            } catch (Exception e) {
                //临时Catch方案，ND8775台架 install的包会crash
                Logger.e(MapDefaultFinalTag.DEFAULT_TAG, "PatacNetClient init failed: " + e.getMessage());
            }
        });
    }

    /**
     * 初始化MCP SDK
     * 使用新SDK替代原有的MCP协调中心和导航服务
     */
    private void initMcpSdk() {
        try {
            // 注册MCP回调（保留给搜索功能）
            BaseToolHelper mcpCallback = BaseToolHelper.getInstance();
            SearchPackage.setMCPSearchCallback(mcpCallback);
            Logger.d(MapDefaultFinalTag.DEFAULT_TAG, "已注册MCP搜索回调");

            // 创建工具实例
            LocationTools locationTools = new LocationTools();
            SearchTools searchTools = new SearchTools();
            NavigationTools navigationTools = new NavigationTools();
            FavoriteTools favoriteTools = new FavoriteTools();
            SettingTools settingTools = new SettingTools();

            // 创建ServerLifecycle实现
            IServerLifecycle serverLifecycle = new IServerLifecycle() {
                @Override
                public void onStateChanged(ConnectionState state) {
                    Logger.d(MapDefaultFinalTag.DEFAULT_TAG, "MCP连接状态变化: " + state);
                }

                @Override
                public void onClientReady(McpClient client) {
                    Logger.d(MapDefaultFinalTag.DEFAULT_TAG, "MCP客户端准备就绪");
                }
            };

            // 使用新MCP SDK的Builder模式初始化
            Mcp.newBuilder(this)
                    .setTools(Arrays.asList(
                            locationTools,
                            searchTools,
                            navigationTools,
                            favoriteTools,
                            settingTools
                    ))
                    .serverLifecycle(serverLifecycle)
                    .build();

            Logger.d(MapDefaultFinalTag.DEFAULT_TAG, "MCP SDK初始化完成");

        } catch (Exception e) {
            Logger.e(MapDefaultFinalTag.DEFAULT_TAG, "MCP SDK初始化失败: " + e.getMessage(), e);
        }
    }
}

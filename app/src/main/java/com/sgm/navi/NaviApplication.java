package com.sgm.navi;

import android.app.Activity;
import android.app.Application;
import android.content.Intent;

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
import com.sgm.navi.service.StartService;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.mcp.tools.SGMNavigationTools;
import com.sgm.navi.ui.BaseApplication;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public class NaviApplication extends BaseApplication implements Application.ActivityLifecycleCallbacks {
    
    @Override
    public void onCreate() {
        super.onCreate();
        Logger.setDefaultTag(MapDefaultFinalTag.DEFAULT_TAG);
        initARouter();
        initComponent();
        startSGMNavigationService();
    }

    @Override
    public void onTerminate() {
        super.onTerminate();
        CarModelsFeature.getInstance().unRegisterBroadcast();
        StartService.getInstance().unSdkInit();
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
     * 启动SGM导航服务
     * 让SGMNavigationService自己负责连接MCP协调中心和注册工具
     */
    private void startSGMNavigationService() {
        try {
            // 注册MCP回调，需要在启动服务前进行，因为SGMNavigationTools实例会被创建
            // 这样SearchPackage就能在geoSearch回调中调用我们的方法
            SGMNavigationTools mcpCallback = new SGMNavigationTools();
            SearchPackage.setMCPGeoSearchCallback(mcpCallback);
            Logger.d(MapDefaultFinalTag.DEFAULT_TAG, "已注册MCP geoSearch回调");
            
            // 1. 先启动MCP协调中心服务
            startMCPCoordinatorService();
            
            // 2. 延迟启动SGM导航服务，确保协调中心先启动
            ThreadManager.getInstance().postDelay(() -> {
                Intent intent = new Intent(this, com.sgm.navi.mcp.SGMNavigationService.class);
                // 使用startForegroundService避免后台启动限制
                if (android.os.Build.VERSION.SDK_INT >= android.os.Build.VERSION_CODES.O) {
                    startForegroundService(intent);
                } else {
                    startService(intent);
                }
                Logger.d(MapDefaultFinalTag.DEFAULT_TAG, "启动SGM导航服务");
            }, 1000); // 延迟1秒启动
            
        } catch (Exception e) {
            Logger.e(MapDefaultFinalTag.DEFAULT_TAG, "启动SGM导航服务失败: " + e.getMessage());
        }
    }
    
    /**
     * 启动MCP协调中心服务
     */
    private void startMCPCoordinatorService() {
        try {
            Intent intent = new Intent(this, com.sgm.navi.mcp.coordinator.MCPCoordinatorService.class);
            if (android.os.Build.VERSION.SDK_INT >= android.os.Build.VERSION_CODES.O) {
                startForegroundService(intent);
            } else {
                startService(intent);
            }
            Logger.d(MapDefaultFinalTag.DEFAULT_TAG, "启动MCP协调中心服务");
        } catch (Exception e) {
            Logger.e(MapDefaultFinalTag.DEFAULT_TAG, "启动MCP协调中心服务失败: " + e.getMessage());
        }
    }
}

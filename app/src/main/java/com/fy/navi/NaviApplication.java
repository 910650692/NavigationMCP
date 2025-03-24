package com.fy.navi;

import android.annotation.SuppressLint;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;

import androidx.core.content.ContextCompat;

import com.alibaba.android.arouter.launcher.ARouter;
import com.android.utils.log.Logger;
import com.fy.navi.burypoint.BuryManager;
import com.fy.navi.burypoint.BuryPointController;
import com.fy.navi.flavor.BaseTestCarType;
import com.fy.navi.flavor.TestCarType;
import com.fy.navi.hmi.BuildConfig;
import com.fy.navi.hmi.navi.NaviGuidanceModel;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.navi.NaviAdapter;
import com.fy.navi.service.adapter.route.RouteAdapter;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.navi.NaviDriveReportEntity;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.ui.BaseApplication;

import java.util.Objects;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public class NaviApplication extends BaseApplication {
    private static final String TAG = "NaviApplication";

    @Override
    public void onCreate() {
        super.onCreate();
        BaseTestCarType testCarType = new TestCarType();
        AppContext.getInstance().setMApplication(this);
        AppContext.getInstance().setMContext(getApplicationContext());
        Logger.setDefaultTag(MapDefaultFinalTag.DEFAULT_TAG);
        initARouter();
        // 初始化SDK
        initSensorsDataAPI();
//        test();
    }

    @SuppressLint("WrongConstant")
    private void test() {
        Logger.i("shisong", "test");
        ContextCompat.registerReceiver(getApplicationContext(), new BroadcastReceiver() {
            @Override
            public void onReceive(Context context, Intent intent) {
                Logger.i("shisong", "onReceive");
                int data = intent.getIntExtra("ss", 0);
                // 测试服务区/收费站信息
//                long status = NaviAdapter.getInstance().obtainSAPAInfo(true);
//                Logger.i("shisong", "status:" + status);
                // 测试行程报告
//                NaviDriveReportEntity naviDriveReportEntity = new NaviDriveReportEntity();
//                NaviDriveReportEntity.NaviStatisticsInfoEntity blNaviStatisticsInfo = new NaviDriveReportEntity.NaviStatisticsInfoEntity();
//                blNaviStatisticsInfo.setDrivenDist(199);
//                blNaviStatisticsInfo.setDrivenTime(199);
//                naviDriveReportEntity.setNaviStatisticsInfoEntity(blNaviStatisticsInfo);
//                NaviPackage.getInstance().onDriveReport(naviDriveReportEntity);
                // 测试全览状态获取
//                NaviPackage.getInstance().addOnPreviewStatusChangeListener(new NaviPackage.OnPreViewStatusChangeListener() {
//                    @Override
//                    public void onPreViewStatusChange(boolean isPreView) {
//                        Logger.i("shisong", "isPreView:" + isPreView);
//                    }
//                });
//                if (data == 1) {
//                    boolean previewStatus = NaviPackage.getInstance().getPreviewStatus();
//                    Logger.i("shisong", "previewStatus:" + previewStatus);
//                }
                // 路况获取测试
//                Logger.i("shisong", "data:" + data);
//                NaviAdapter.getInstance().test();
//                if (data == 1) {
//                    NaviPackage.getInstance().getTmcStatus("陆家嘴水环-码头", null, null,
//                            MapTypeId.MAIN_SCREEN_MAIN_MAP);
//                } else if (data == 2) {
//                    NaviPackage.getInstance().getTmcStatus(null, "滴水湖馨苑", "陆家嘴水环-码头",
//                            MapTypeId.MAIN_SCREEN_MAIN_MAP);
//                }
            }
        }, new IntentFilter("shi.song"), ContextCompat.RECEIVER_EXPORTED);
    }

    @Override
    public void onTerminate() {
        super.onTerminate();
        Logger.i(TAG, "onTerminate");
        NaviService.exitProcess();
    }

    private void initARouter() {
        if (BuildConfig.DEBUG) {
            ARouter.openLog();
            ARouter.openDebug();
        }
        ARouter.init(this);
    }

    private void initSensorsDataAPI() {
        BuryManager.getInstance().initSensorsDataAPI(getApplicationContext());
    }
}

package com.fy.navi.burypoint;

import android.content.Context;

import com.android.utils.log.Logger;
import com.bigtimes.sdk.AlarmModeEnum;
import com.bigtimes.sdk.BuriedPointModeEnum;
import com.bigtimes.sdk.ModeEnum;
import com.bigtimes.sdk.OASAPI;
import com.sensorsdata.analytics.android.sdk.SAConfigOptions;

import org.json.JSONObject;

public class BuryManager {

    private static final String TAG = "BuryManager";

    private static volatile BuryManager instance;

    public static BuryManager getInstance(){
        if (instance == null){
            synchronized (BuryManager.class){
                if (instance == null){
                    instance = new BuryManager();
                }
            }
        }
        return instance;
    }

    public void initSensorsDataAPI(Context context){
        SAConfigOptions configOptions = new SAConfigOptions();

        // 设置埋点模式
        // UNIFIED_SERVICE：统一服务
        // INTEGRATION：独立集成）
        configOptions.buriedPointMode(BuriedPointModeEnum.INTEGRATION);

        // 设置SDK的模式，如DEV PROD等
        // DEV模式为开发模式，能够输出调试信息，使用枚举定义这两种模式
        configOptions.mode(ModeEnum.DEV);

        // 设置预警
        configOptions.alarm(AlarmModeEnum.LOG);

        // 每缓存 ？ 条日志发送一次
        configOptions.setFlushBulkSize(50);
        // 设置每 ？ 毫秒发送一次
        configOptions.setFlushInterval(5 * 1000);
        // 设置本地数据缓存上限值为 16 MB
        configOptions.setMaxCacheSize(16 * 1024 * 1024);

        // 初始化 SDK
        OASAPI.startWithConfigOptions(context, configOptions);

        getPresetProperties();
    }

    private void getPresetProperties(){
        JSONObject presetProperties = OASAPI.sharedInstance().getPresetProperties();
        Logger.i(TAG, "presetProperties: " + presetProperties.toString());
    }

}

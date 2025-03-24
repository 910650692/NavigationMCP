package com.fy.navi.service.adapter.cruise;

import com.android.utils.log.Logger;
import com.fy.navi.service.AdapterConfig;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.navi.NaviAdapter;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.adapter.setting.SettingAdapter;
import com.fy.navi.service.define.cruise.CruiseParamEntity;

import java.util.Objects;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/5
 */
public class CruiseAdapter {
    private static final String TAG = MapDefaultFinalTag.CRUISE_SERVICE_TAG;
    private static final String NAVI_PKG_NAME =
            Objects.requireNonNull(CruiseAdapter.class.getPackage()).getName();
    private static final String NAVI_CLS_NAME = "CruiseAdapterApiImpl";
    private ICruiseApi mCruiseApi;
    private SettingAdapter mSettingAdapter;
    private NaviAdapter mNaviAdapter;

    private CruiseAdapter() {
        mCruiseApi = (ICruiseApi) AdapterConfig.getObject(NAVI_PKG_NAME, NAVI_CLS_NAME);
        mSettingAdapter = SettingAdapter.getInstance();
        mNaviAdapter = NaviAdapter.getInstance();
    }

    public void initCruise() {
        mCruiseApi.initCruise();
    }

    public void registerObserver(String key, CruiseObserver cruiseObserver) {
        mCruiseApi.registerObserver(key, cruiseObserver);
    }

    public void unregisterObserver(String key) {
        mCruiseApi.unregisterObserver(key);
    }

    public void unInitCruise() {
        mCruiseApi.unInitCruise();
    }

    public boolean startCruise() {
        updateCruiseBroadcastParam(mSettingAdapter.getConfigKeyRoadWarn(), mSettingAdapter.getConfigKeySafeBroadcast(),
                mSettingAdapter.getConfigKeyDriveWarn());
        setTrParam();
        return mCruiseApi.startCruise();
    }

    public void setConfigKeyRoadWarn(boolean roadWarn) {
        updateCruiseBroadcastParam(roadWarn, mSettingAdapter.getConfigKeySafeBroadcast(),
                mSettingAdapter.getConfigKeyDriveWarn());
    }

    public void setConfigKeySafeBroadcast(boolean safeBroadcast) {
        updateCruiseBroadcastParam(mSettingAdapter.getConfigKeyRoadWarn(), safeBroadcast,
                mSettingAdapter.getConfigKeyDriveWarn());
    }

    public void setConfigKeyDriveWarn(boolean driveWarn) {
        updateCruiseBroadcastParam(mSettingAdapter.getConfigKeyRoadWarn(), mSettingAdapter.getConfigKeySafeBroadcast(),
                driveWarn);
    }

    /***更新巡航播报***/
    private void updateCruiseBroadcastParam(boolean roadWarn, boolean safeBroadcast, boolean driveWarn) {
        Logger.i(TAG, "roadWarn: " + roadWarn + ",safeBroadcast：" + safeBroadcast + ",driveWarn：" + driveWarn);
        //默认同时关闭
        int mode = 0;
        //前方路况 0：off； 1：on
        int mConfigKeyRoadWarn = roadWarn ? 1 : 0;
        //摄像头播报 0：off； 1：on
        int mConfigKeySafeBroadcast = safeBroadcast ? 1 : 0;
        //安全提醒 0：off； 1：on
        int mConfigKeyDriveWarn = driveWarn ? 1 : 0;
        //播报开启时，需要同时开启TR开关
        if (mConfigKeyRoadWarn == 0 && mConfigKeySafeBroadcast == 0 && mConfigKeyDriveWarn == 0) {//同时关闭
            mode = 0;
        } else if (mConfigKeyRoadWarn == 1 && mConfigKeySafeBroadcast == 0 && mConfigKeyDriveWarn == 0) {//前方路况
            mode = 4;
        } else if (mConfigKeyRoadWarn == 0 && mConfigKeySafeBroadcast == 1 && mConfigKeyDriveWarn == 0) {//电子眼
            mode = 1;
        } else if (mConfigKeyRoadWarn == 0 && mConfigKeySafeBroadcast == 0 && mConfigKeyDriveWarn == 1) {//安全提醒
            mode = 2;
        } else if (mConfigKeyRoadWarn == 0 && mConfigKeySafeBroadcast == 1 && mConfigKeyDriveWarn == 1) {//电子眼+安全提醒
            mode = 3;
        } else if (mConfigKeyRoadWarn == 1 && mConfigKeySafeBroadcast == 1 && mConfigKeyDriveWarn == 0) {//前方路况+电子眼
            mode = 5;
        } else if (mConfigKeyRoadWarn == 1 && mConfigKeySafeBroadcast == 0 && mConfigKeyDriveWarn == 1) {//前方路况+安全提醒
            mode = 6;
        } else if (mConfigKeyRoadWarn == 1 && mConfigKeySafeBroadcast == 1 && mConfigKeyDriveWarn == 1) {//前方路况+电子眼+安全提醒
            mode = 7;
        }
        CruiseParamEntity cruiseParamEntity = new CruiseParamEntity()
                .setType(NaviConstant.GuideParamType.GUIDE_PARAM_CRUISE)
                .setCameraNum(3L)
                .setMode(mode);
        mNaviAdapter.setCruiseParam(cruiseParamEntity);
    }

    /***设置TrParam***/
    public void setTrParam() {
        CruiseParamEntity trParamEntity = new CruiseParamEntity()
                .setType(NaviConstant.GuideParamType.GUIDE_PARAM_TR)
                .setTrEnable(true);
        mNaviAdapter.setCruiseParam(trParamEntity);
    }

    /***反初始化巡航参数***/
    public void unInitCruiseParam() {
        CruiseParamEntity cruiseParamEntity = new CruiseParamEntity()
                .setType(NaviConstant.GuideParamType.GUIDE_PARAM_CRUISE)
                .setCameraNum(3L)
                .setMode(0);
        mNaviAdapter.setCruiseParam(cruiseParamEntity);

        CruiseParamEntity trParamEntity = new CruiseParamEntity()
                .setType(NaviConstant.GuideParamType.GUIDE_PARAM_TR)
                .setTrEnable(false);
        mNaviAdapter.setCruiseParam(trParamEntity);
    }

    public boolean stopCruise() {
        unInitCruiseParam();
        return mCruiseApi.stopCruise();
    }

    public static CruiseAdapter getInstance() {
        return CruiseAdapter.Helper.ra;
    }

    private static final class Helper {
        private static final CruiseAdapter ra = new CruiseAdapter();
    }
}

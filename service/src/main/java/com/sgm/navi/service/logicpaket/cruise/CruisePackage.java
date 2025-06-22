package com.sgm.navi.service.logicpaket.cruise;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.burypoint.controller.BuryPointController;
import com.sgm.navi.service.adapter.cruise.CruiseAdapter;
import com.sgm.navi.service.adapter.cruise.CruiseObserver;
import com.sgm.navi.service.adapter.layer.LayerAdapter;
import com.sgm.navi.service.adapter.navistatus.NavistatusAdapter;
import com.sgm.navi.service.adapter.speech.SpeechAdapter;
import com.sgm.navi.service.define.cruise.CruiseFacilityEntity;
import com.sgm.navi.service.define.cruise.CruiseInfoEntity;
import com.sgm.navi.service.define.layer.refix.DynamicLevelMode;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.navi.LaneInfoEntity;
import com.sgm.navi.service.define.navi.SoundInfoEntity;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.setting.SettingPackage;

import java.util.Hashtable;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public class CruisePackage implements CruiseObserver {
    private static final String TAG = "CruisePackage";
    private CruiseAdapter mCruiseAdapter;
    private final LayerAdapter mLayerAdapter;
    private Hashtable<String, ICruiseObserver> mCruiseObserver;
    private NavistatusAdapter mNavistatusAdapter;
    private SpeechAdapter mSpeechAdapter;

    private CruisePackage() {
        mCruiseObserver = new Hashtable<>();
        mCruiseAdapter = CruiseAdapter.getInstance();
        mLayerAdapter = LayerAdapter.getInstance();
        mSpeechAdapter = SpeechAdapter.getInstance();
        mNavistatusAdapter = NavistatusAdapter.getInstance();
    }

    public void initCruise() {
        mCruiseAdapter.initCruise();
        mCruiseAdapter.registerObserver("CruisePackage", this);
    }

    public void registerObserver(String key, ICruiseObserver cruiseObserver) {
        mCruiseObserver.put(key, cruiseObserver);
    }

    public void unregisterObserver(String key) {
        mCruiseObserver.remove(key);
    }

    public void unInitCruise() {
        mCruiseAdapter.unInitCruise();
    }

    /***
     * 开始巡航
     *
     * @return
     */
    public boolean startCruise() {
        final boolean result = mCruiseAdapter.startCruise();
        if (result) {
            Logger.d(TAG, "巡航开启成功");
            mLayerAdapter.setFollowMode(MapType.MAIN_SCREEN_MAIN_MAP, true);
            mNavistatusAdapter.setNaviStatus(NaviStatus.NaviStatusType.CRUISE);
            mLayerAdapter.setVisibleCruiseSignalLight(MapType.MAIN_SCREEN_MAIN_MAP, true);
            initScaleSize();
            sendBuryPointForCruise(true);
        }
        return result;
    }

    /***
     * ·若设置页【自动比例尺】选项关闭，则采用地图默认比例尺2D: 500m, 3D:50m, 允许用户手动调节比例尺与俯仰角从而锁定巡航视角。;
     * 若设置页【自动比例尺】选项开启，比例尺的范围是 20~200m。速度与比例尺的对应关系参照PIS-2116第3.2.11.3章节。
     */
    private void initScaleSize() {
        final boolean isAuto = SettingPackage.getInstance().getAutoScale();
        final boolean is3D = SettingPackage.getInstance().getConfigKeyMapviewMode() == 2;
        Logger.i(TAG, "initScaleSize", "isAuto:" + isAuto, "is3D视角：" + is3D);
        if (!isAuto) {
            if (is3D) {
                MapPackage.getInstance().setZoomLevel(MapType.MAIN_SCREEN_MAIN_MAP, 17f);
            } else {
                MapPackage.getInstance().setZoomLevel(MapType.MAIN_SCREEN_MAIN_MAP, 14f);
            }
        } else {
            // TODO 如果动态比例尺开启，高德不建议设置范围
            // 在执行一次，防止缓存导致动态比例尺未生效
            mLayerAdapter.openDynamicLevel(MapType.MAIN_SCREEN_MAIN_MAP, DynamicLevelMode.DYNAMIC_LEVEL_CRUISE);
        }
    }

    /*结束巡航*/
    public boolean stopCruise() {
        boolean isSuccess = mCruiseAdapter.stopCruise();
        if (isSuccess) {
            mSpeechAdapter.stop();
            mLayerAdapter.setFollowMode(MapType.MAIN_SCREEN_MAIN_MAP, false);
            mNavistatusAdapter.setNaviStatus(NaviStatus.NaviStatusType.NO_STATUS);
            sendBuryPointForCruise(false);
        }
        Logger.i(TAG, "stopCruise", "result:" + isSuccess, "currentNaviStatus:" + mNavistatusAdapter.getCurrentNaviStatus());
        return isSuccess;
    }

    @Override
    public void onCruiseLaneInfo(boolean isShowLane, LaneInfoEntity laneInfo) {
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mCruiseObserver)) {
                for (ICruiseObserver cruiseObserver : mCruiseObserver.values()) {
                    cruiseObserver.onUpdateCruiseInfo(isShowLane, laneInfo);
                }
            }
        });
    }

    @Override
    public void onShowCruiseCameraExt(CruiseInfoEntity cruiseInfoEntity) {
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mCruiseObserver)) {
                for (ICruiseObserver cruiseObserver : mCruiseObserver.values()) {
                    cruiseObserver.onShowCruiseCameraExt(cruiseInfoEntity);
                }
            }
        });
    }

    @Override
    public void onUpdateCruiseInfo(CruiseInfoEntity cruiseInfoEntity) {
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mCruiseObserver)) {
                for (ICruiseObserver cruiseObserver : mCruiseObserver.values()) {
                    cruiseObserver.onUpdateCruiseInfo(cruiseInfoEntity);
                }
            }
        });
    }

    @Override
    public void setConfigKeyRoadWarn(boolean roadWarn) {
        mCruiseAdapter.setConfigKeyRoadWarn(roadWarn);
    }

    @Override
    public void setConfigKeySafeBroadcast(boolean safeBroadcast) {
        mCruiseAdapter.setConfigKeySafeBroadcast(safeBroadcast);
    }

    @Override
    public void setConfigKeyDriveWarn(boolean driveWarn) {
        mCruiseAdapter.setConfigKeyDriveWarn(driveWarn);
    }

    @Override
    public void onUpdateCruiseFacility(CruiseFacilityEntity cruiseFacilityEntity) {

    }

    @Override
    public void onNaviStop() {
        ThreadManager.getInstance().postUi(() -> {
            mNavistatusAdapter.setNaviStatus(NaviStatus.NaviStatusType.NO_STATUS);
            if (!ConvertUtils.isEmpty(mCruiseObserver)) {
                for (ICruiseObserver cruiseObserver : mCruiseObserver.values()) {
                    if (cruiseObserver != null) {
                        cruiseObserver.onNaviStop();
                    }
                }
            }
        });
    }

    @Override
    public void onPlayTTS(SoundInfoEntity info) {
        // 这里不需要处理，统一放在NaviPackage里面处理
    }

    @Override
    public void onPlayRing(int type) {

    }

    @HookMethod
    private void sendBuryPointForCruise(boolean isEnter) {
        String eventName = isEnter ? BuryConstant.EventName.AMAP_CRUISE_ENTER : BuryConstant.EventName.AMAP_CRUISE_EXIT;
        BuryPointController.getInstance().setEventName(eventName);
    }

    public static CruisePackage getInstance() {
        return Helper.ep;
    }

    private static final class Helper {
        private static final CruisePackage ep = new CruisePackage();
    }
}

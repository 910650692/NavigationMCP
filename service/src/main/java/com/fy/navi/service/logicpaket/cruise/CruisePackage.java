package com.fy.navi.service.logicpaket.cruise;

import com.android.utils.ConvertUtils;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.adapter.cruise.CruiseAdapter;
import com.fy.navi.service.adapter.cruise.CruiseObserver;
import com.fy.navi.service.adapter.layer.LayerAdapter;
import com.fy.navi.service.adapter.navistatus.NavistatusAdapter;
import com.fy.navi.service.adapter.speech.SpeechAdapter;
import com.fy.navi.service.define.cruise.CruiseInfoEntity;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.SoundInfoEntity;
import com.fy.navi.service.define.navistatus.NaviStatus;

import java.util.Hashtable;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public class CruisePackage implements CruiseObserver {
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
        boolean result = mCruiseAdapter.startCruise();
        if (result) {
            mLayerAdapter.setFollowMode(MapTypeId.MAIN_SCREEN_MAIN_MAP, true);
            mNavistatusAdapter.setNaviStatus(NaviStatus.NaviStatusType.CRUISE);
            mLayerAdapter.setVisibleCruiseSignalLight(MapTypeId.MAIN_SCREEN_MAIN_MAP, true);
        }
        return result;
    }

    /*结束巡航*/
    public boolean stopCruise() {
        mSpeechAdapter.stop();
        mLayerAdapter.setFollowMode(MapTypeId.MAIN_SCREEN_MAIN_MAP, false);
        boolean isSuccess = mCruiseAdapter.stopCruise();
        if (isSuccess) {
            mNavistatusAdapter.setNaviStatus(NaviStatus.NaviStatusType.NO_STATUS);
        }
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

    }

    public static CruisePackage getInstance() {
        return Helper.ep;
    }

    private static final class Helper {
        private static final CruisePackage ep = new CruisePackage();
    }
}

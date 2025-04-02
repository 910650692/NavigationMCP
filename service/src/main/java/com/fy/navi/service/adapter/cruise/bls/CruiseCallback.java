package com.fy.navi.service.adapter.cruise.bls;

import com.android.utils.ConvertUtils;
import com.autonavi.gbl.guide.model.CruiseInfo;
import com.autonavi.gbl.guide.model.LaneInfo;
import com.autonavi.gbl.guide.model.NaviCameraExt;
import com.autonavi.gbl.guide.model.SoundInfo;
import com.autonavi.gbl.guide.observer.ICruiseObserver;
import com.autonavi.gbl.guide.observer.ISoundPlayObserver;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.cruise.CruiseObserver;
import com.fy.navi.service.adapter.navi.bls.NaviDataFormatHelper;
import com.fy.navi.service.define.navi.LaneInfoEntity;

import java.util.ArrayList;
import java.util.Hashtable;

/**
 * @Description 巡航电子眼观察者
 * @Author lvww
 * @date 2024/12/5
 */
public class CruiseCallback implements ICruiseObserver, ISoundPlayObserver{
    private static final String TAG = MapDefaultFinalTag.NAVI_SERVICE_TAG;
    private Hashtable<String, CruiseObserver> mCruiseObservers;

    public CruiseCallback(Hashtable<String, CruiseObserver> cruiseObservers) {
        this.mCruiseObservers = cruiseObservers;
    }

    @Override
    public void onShowCruiseLaneInfo(LaneInfo info) { // 显示巡航车道线信息
        LaneInfoEntity laneInfoEntity = NaviDataFormatHelper.forMatLaneInfo(info);
        if (!ConvertUtils.isEmpty(mCruiseObservers)) {
            for (CruiseObserver cruiseObserver : mCruiseObservers.values()) {
                if (cruiseObserver != null) {
                    cruiseObserver.onCruiseLaneInfo(!ConvertUtils.isNull(laneInfoEntity), laneInfoEntity);
                }
            }
        }
    }

    @Override
    public void onHideCruiseLaneInfo() {
        if (!ConvertUtils.isEmpty(mCruiseObservers)) {
            for (CruiseObserver cruiseObserver : mCruiseObservers.values()) {
                if (cruiseObserver != null) {
                    cruiseObserver.onCruiseLaneInfo(false, null);
                }
            }
        }
    }


    @Override
    public void onShowCruiseCameraExt(ArrayList<NaviCameraExt> cameraInfoList) {
        if (!ConvertUtils.isEmpty(mCruiseObservers)) {
            for (CruiseObserver cruiseObserver : mCruiseObservers.values()) {
                if (cruiseObserver != null) {
                    cruiseObserver.onShowCruiseCameraExt(NaviDataFormatHelper.formatCruiseCameraExt(cameraInfoList));
                }
            }
        }
    }


    /**
     * 巡航过程中传出巡航状态下的信息
     * thread mutil
     *
     * @param noNaviInfor 巡航信息
     *                    1、只有GCKAutoFlag开关开启才会回调
     *                    2、需要有离线数据，并且定位匹配到道路
     */
    @Override
    public void onUpdateCruiseInfo(CruiseInfo noNaviInfor) {
        if (!ConvertUtils.isEmpty(mCruiseObservers)) {
            for (CruiseObserver cruiseObserver : mCruiseObservers.values()) {
                if (cruiseObserver != null) {
                    cruiseObserver.onUpdateCruiseInfo(NaviDataFormatHelper.formatCruiseInfo(noNaviInfor));
                }
            }
        }
    }

    @Override
    public void onPlayTTS(final SoundInfo soundInfo) {
        ISoundPlayObserver.super.onPlayTTS(soundInfo);
        if (!ConvertUtils.isEmpty(mCruiseObservers)) {
            for (CruiseObserver cruiseObserver : mCruiseObservers.values()) {
                if (cruiseObserver != null) {
                    cruiseObserver.onPlayTTS(NaviDataFormatHelper.formatSoundInfo(soundInfo));
                }
            }
        }
    }

    @Override
    public void onPlayRing(int type) {
        ISoundPlayObserver.super.onPlayRing(type);
        if (!ConvertUtils.isEmpty(mCruiseObservers)) {
            for (CruiseObserver cruiseObserver : mCruiseObservers.values()) {
                if (cruiseObserver != null) {
                    cruiseObserver.onPlayTTS(NaviDataFormatHelper.formatSoundInfo(type));
                }
            }
        }
    }

    @Override
    public boolean isPlaying() {
        return false;
    }
}

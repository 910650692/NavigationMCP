package com.fy.navi.service.adapter.navi;

import android.graphics.Rect;
import android.text.TextUtils;

import com.autonavi.gbl.guide.model.QueryLanesInfo;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.autonavi.gbl.common.path.model.ViaPointInfo;
import com.autonavi.gbl.common.path.option.PathInfo;
import com.autonavi.gbl.common.path.option.SegmentInfo;
import com.fy.navi.service.AdapterConfig;
import com.fy.navi.service.adapter.layer.LayerAdapter;
import com.fy.navi.service.adapter.navistatus.NavistatusAdapter;
import com.fy.navi.service.adapter.route.RouteAdapter;
import com.fy.navi.service.adapter.setting.SettingAdapter;
import com.fy.navi.service.define.cruise.CruiseParamEntity;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviParamEntity;
import com.fy.navi.service.define.navi.NaviStartType;
import com.fy.navi.service.define.navi.NaviTmcInfo;
import com.fy.navi.service.define.navi.SoundInfoEntity;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.logicpaket.setting.SettingPackage;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/5
 */
public class NaviAdapter {

    public static final String TAG = "NaviAdapter";
    private static final String NAVI_PKG_NAME =
            Objects.requireNonNull(NaviAdapter.class.getPackage()).getName();
    private static final String NAVI_CLS_NAME = "NaviAdapterApiImpl";
    private INaviApi mNaviApi;
    private final Map<MapTypeId, Rect> mRoadCrossRect = new HashMap<>();
    private LayerAdapter mLayerAdapter;
    private NavistatusAdapter mNavistatusAdapter;
    private SettingAdapter mSettingAdapter;
    private RouteAdapter mRouteAdapter;
    // 本地存储路导航路况数据
    private NaviTmcInfo mNaviTmcInfo;
    // 本地存储导航数据
    private NaviEtaInfo mNaviEtaInfo;
    // 本地存储tts数据
    private SoundInfoEntity mSoundInfoEntity;
    public CountDownLatch mCountDownLatch;
    public static final int THREE_SECONDS = 3 * 1000;

    private NaviAdapter() {
        mNaviApi = (INaviApi) AdapterConfig.getObject(NAVI_PKG_NAME, NAVI_CLS_NAME);
        mLayerAdapter = LayerAdapter.getInstance();
        mNavistatusAdapter = NavistatusAdapter.getInstance();
        mSettingAdapter = SettingAdapter.getInstance();
        mRouteAdapter = RouteAdapter.getInstance();
    }

    public void initNaviService() {
        mNaviApi.initNaviService();
    }

    public void registerObserver(String key, GuidanceObserver guidanceObserver) {
        mNaviApi.registerObserver(key, guidanceObserver);
    }

    public void unregisterObserver(String key) {
        mNaviApi.unregisterObserver(key);
    }

    public void unInitNaviService() {
        mNaviApi.unInitNaviService();
    }

    public long obtainSAPAInfo(boolean isFindRemainPath) {
        return mNaviApi.obtainSAPAInfo(isFindRemainPath);
    }

    public void selectMainPathID(long pathID) {
        mNaviApi.selectMainPathID(pathID);
    }

    public boolean startNavigation(NaviStartType naviStartType) {
        updateBroadcastParam(mSettingAdapter.getConfigKeyBroadcastMode(), true);
        return mNaviApi.startNavigation(naviStartType);
    }

    public boolean stopNavigation() {
        return mNaviApi.stopNavigation();
    }

    public void setNaviPath(int routeIndex, RouteLineLayerParam routeLineLayerParam) {
        mNaviApi.setNaviPath(routeIndex, routeLineLayerParam);
    }

    public void updateNaviPath(int routeIndex, RouteLineLayerParam routeLineLayerParam) {
        mNavistatusAdapter.setNaviStatus(NaviStatus.NaviStatusType.NAVING);
        mNaviApi.updateNaviPath(routeIndex, routeLineLayerParam);
        mLayerAdapter.updateGuideCarStyle(MapTypeId.MAIN_SCREEN_MAIN_MAP);
        mLayerAdapter.updateGuideCarStyle(MapTypeId.LAUNCHER_WIDGET_MAP);
        mLayerAdapter.updateGuideCarStyle(MapTypeId.LAUNCHER_DESK_MAP);
    }

    public void setRoadCrossRect(MapTypeId surfaceViewId, Rect rect) {
        mRoadCrossRect.put(surfaceViewId, rect);
    }

    public Rect gettRoadCrossRect(MapTypeId surfaceViewId) {
        return mRoadCrossRect.get(surfaceViewId);
    }

    public static NaviAdapter getInstance() {
        return Helper.ra;
    }

    public void setCruiseParam(CruiseParamEntity cruiseParamEntity) {
        mNaviApi.setCruiseParam(cruiseParamEntity);
    }

    public void updateBroadcastParam(int broadcastType, boolean isDay) {
        NaviParamEntity naviParamEntity = new NaviParamEntity()
                .setType(NaviConstant.GuideParamType.GuideParamTTSPlay)
                .setDay(isDay)
                .setEnableADCode(true)
                .setFatiguedTTS(2);
        if (broadcastType == NaviConstant.BroadcastType.BROADCAST_CONCISE) {
            naviParamEntity.setStyle(4);
            SettingPackage.getInstance().setConfigKeyBroadcastMode(1);
        } else if (broadcastType == NaviConstant.BroadcastType.BROADCAST_MINIMALISM) {
            naviParamEntity.setStyle(6);
            SettingPackage.getInstance().setConfigKeyBroadcastMode(3);
        } else {
            naviParamEntity.setStyle(2);
            SettingPackage.getInstance().setConfigKeyBroadcastMode(2);
        }
        mNaviApi.updateGuideParam(naviParamEntity);
    }

    public void queryAppointLanesInfo(int segmentIdx, int linkIdx) {
        mNaviApi.queryAppointLanesInfo(segmentIdx, linkIdx);
    }

    private static final class Helper {
        private static final NaviAdapter ra = new NaviAdapter();
    }

    /**
     * 设置TMC数据
     * @param naviTmcInfo NaviTmcInfo
     */
    public void setTmcData(NaviTmcInfo naviTmcInfo) {
        Logger.i(TAG, "setTmcData");
        mNaviTmcInfo = naviTmcInfo;
    }

    /**
     * 设置导航数据数据
     * @param naviEtaInfo NaviEtaInfo
     */
    public void setNaviEtaInfo(NaviEtaInfo naviEtaInfo) {
        Logger.i(TAG, "setNaviEtaInfo");
        mNaviEtaInfo = naviEtaInfo;
    }

    public void setSoundInfoEntity(SoundInfoEntity infoEntity) {
        Logger.i(TAG, "setSoundInfoEntity");
        mSoundInfoEntity = infoEntity;
    }

    /**
     * 三个参数，地点/路段a、起点b、终点c，a不空其他为空就是查询地点或道路的路况，a为空bc不为空就是查询b到c的路况，
     * ab为空c不空就是查询当前到c的路况，abc都为空就是查询前方路况
     * @param a String
     * @param b String
     * @param c String
     * @return int 交通状态回调 -1：无数据 0:未知状态 1:通畅 2:缓慢 3:拥堵 4:严重拥堵 5:极度通畅
     */
    public int getTmcStatus(String a, String b, String c, MapTypeId mapTypeId) {
        Logger.i(TAG, "getTmcStatus a:" + a + " b:" + b + " c:" + c + " mapTypeId:" +
                mapTypeId);
        PathInfo pathInfo = (PathInfo) mRouteAdapter.getCurrentPath(mapTypeId).getPathInfo();
        if (null == pathInfo) {
            Logger.i(TAG, "getTmcStatus pathInfo is null");
            return -1;
        }
        // a不空其他为空就是查询地点或道路的路况
        if (!TextUtils.isEmpty(a) && TextUtils.isEmpty(b) && TextUtils.isEmpty(c)) {
            return getPointTmcStatus(a, pathInfo);
        }
        // a为空bc不为空就是查询b到c的路况
        if (TextUtils.isEmpty(a) && !TextUtils.isEmpty(b) && !TextUtils.isEmpty(c)) {
            return getDistanceTmcStatus(b, c, pathInfo);
        }
        // ab为空c不空就是查询当前到c的路况
        if (TextUtils.isEmpty(a) && TextUtils.isEmpty(b) && !TextUtils.isEmpty(c)) {
            return getToViaPointTmcStatus(c, pathInfo);
        }
        return -1;
    }

    //abc都为空就是查询前方路况
    public String getFrontTmcStatus() {
        try {
            mCountDownLatch = new CountDownLatch(1);
            // 主动获取前方的路况信息
            mNaviApi.playTRManualExt(0);
            // 延迟3秒后看是否获取到了tts数据
            ThreadManager.getInstance().asyncDelay(runnable, THREE_SECONDS, TimeUnit.MILLISECONDS);
            mCountDownLatch.await();
            if (mSoundInfoEntity != null) {
                return mSoundInfoEntity.getText();
            }
            mCountDownLatch = null;
        } catch (InterruptedException e) {
            Logger.e(TAG, "getTmcStatus InterruptedException", e.getMessage());
        }
        return "";
    }

    private final Runnable runnable = new Runnable() {
        @Override
        public void run() {
            if (mCountDownLatch != null) {
                long count = mCountDownLatch.getCount();
                if (count != 0) {
                    mSoundInfoEntity = null;
                    mCountDownLatch.countDown();
                }
            }
        }
    };

    /**
     * 获取途经点对应的路段索引
     * @param viaName 途经点名称
     * @param pathInfo 路径信息
     * @return 途经点对应的路段索引
     */
    private short getViaSegIdx(String viaName, PathInfo pathInfo) {
        List<ViaPointInfo> viaPointInfoList =  pathInfo.getViaPointInfo();
        for (ViaPointInfo viaPointInfo : viaPointInfoList) {
            if (viaName.equals(viaPointInfo.poiName)) {
                return viaPointInfo.segmentIdx;
            }
        }
        return -1;
    }

    /**
     * 获取对应路径的路况信息
     * @param pathInfo 路径信息
     * @return 路径上的路况信息
     */
    private NaviTmcInfo.NaviLightBarInfo getCurrentLightBarInfo(
            ArrayList<NaviTmcInfo.NaviLightBarInfo> naviLightBarInfoList, PathInfo pathInfo) {
        for (NaviTmcInfo.NaviLightBarInfo naviLightBarInfo : naviLightBarInfoList) {
            if (naviLightBarInfo.pathID == pathInfo.getPathID()) {
                return naviLightBarInfo;
            }
        }
        return null;
    }

    /**
     * 获取当点周边的交通情况
     * @param viaName 途经点名称
     * @param pathInfo 路径信息
     * @return 交通状态
     */
    private int getPointTmcStatus(String viaName, PathInfo pathInfo) {
        short viaSegmentIdx = getViaSegIdx(viaName, pathInfo);
        Logger.i(TAG, "getPointTmcStatus viaSegmentIdx:" + viaSegmentIdx);
        if (viaSegmentIdx < 0) {
            return -1;
        }
        if (mNaviTmcInfo != null) {
            NaviTmcInfo.NaviLightBarInfo currentLightBarInfo = getCurrentLightBarInfo(
                    mNaviTmcInfo.lightBarInfo, pathInfo);
            // 获取到途经点的路况信息并返回
            if (currentLightBarInfo != null) {
                ArrayList<NaviTmcInfo.NaviLightBarItem> naviLightBarItems = currentLightBarInfo.
                        itemList;
                for (NaviTmcInfo.NaviLightBarItem naviLightBarItem : naviLightBarItems) {
                    if (viaSegmentIdx >= naviLightBarItem.startSegmentIdx &&
                            viaSegmentIdx <= naviLightBarItem.endSegmentIdx) {
                        if (naviLightBarItem.statusFlag == 0x00) {
                            return naviLightBarItem.status;
                        } else {
                            return naviLightBarItem.fineStatus;
                        }
                    }
                }
            }
        }
        return -1;
    }

    /**
     * 获取一段路线的交通状态 比如从A点到B点的交通状态
     */
    private int getDistanceTmcStatus(String a, String b, PathInfo pathInfo) {
        short viaASegmentIdx = getViaSegIdx(a, pathInfo);
        short viaBSegmentIdx = getViaSegIdx(b, pathInfo);

        Logger.i(TAG, "getPointTmcStatus viaASegmentIdx:" + viaASegmentIdx +
                ",viaBSegmentIdx:" + viaBSegmentIdx);
        if (viaASegmentIdx < 0 || viaBSegmentIdx < 0) {
            return -1;
        }
        if (mNaviTmcInfo != null) {
            NaviTmcInfo.NaviLightBarInfo currentLightBarInfo = getCurrentLightBarInfo(
                    mNaviTmcInfo.lightBarInfo, pathInfo);
            // 获取到途经路段的路况信息并返回
            if (currentLightBarInfo != null) {
                ArrayList<NaviTmcInfo.NaviLightBarItem> naviLightBarItems = currentLightBarInfo.
                        itemList;
                for (NaviTmcInfo.NaviLightBarItem naviLightBarItem : naviLightBarItems) {
                    if ((viaASegmentIdx < naviLightBarItem.endSegmentIdx &&
                            viaASegmentIdx >= naviLightBarItem.startSegmentIdx) ||
                            (viaBSegmentIdx < naviLightBarItem.endSegmentIdx &&
                                    viaBSegmentIdx >= naviLightBarItem.startSegmentIdx)) {
                        if (naviLightBarItem.statusFlag == 0x00) {
                            return naviLightBarItem.status;
                        } else {
                            return naviLightBarItem.fineStatus;
                        }
                    }
                }
            }
        }
        return -1;
    }

    /**
     * 获取当前到途经点的交通状态
     * @param viaName 途经点名称
     * @param pathInfo pathInfo
     */
    private int getToViaPointTmcStatus(String viaName, PathInfo pathInfo) {
        short viaSegmentIdx = getViaSegIdx(viaName, pathInfo);
        int currentSegmentIdx = -1;
        if (mNaviEtaInfo != null) {
            currentSegmentIdx = mNaviEtaInfo.curSegIdx;
        }
        Logger.i(TAG, "getPointTmcStatus viaSegmentIdx:" + viaSegmentIdx);
        if (viaSegmentIdx < 0 || currentSegmentIdx < 0) {
            return -1;
        }
        if (mNaviTmcInfo != null) {
            NaviTmcInfo.NaviLightBarInfo currentLightBarInfo = getCurrentLightBarInfo(
                    mNaviTmcInfo.lightBarInfo, pathInfo);
            // 获取到途经路段的路况信息并返回
            if (currentLightBarInfo != null) {
                ArrayList<NaviTmcInfo.NaviLightBarItem> naviLightBarItems = currentLightBarInfo.
                        itemList;
                for (NaviTmcInfo.NaviLightBarItem naviLightBarItem : naviLightBarItems) {
                    if ((currentSegmentIdx < naviLightBarItem.endSegmentIdx &&
                            currentSegmentIdx >= naviLightBarItem.startSegmentIdx) ||
                            (viaSegmentIdx < naviLightBarItem.endSegmentIdx &&
                                    viaSegmentIdx >= naviLightBarItem.startSegmentIdx)) {
                        if (naviLightBarItem.statusFlag == 0x00) {
                            return naviLightBarItem.status;
                        } else {
                            return naviLightBarItem.fineStatus;
                        }
                    }
                }
            }
        }
        return -1;
    }
}

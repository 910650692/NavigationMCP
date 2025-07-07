package com.sgm.navi.service.adapter.navi;

import android.text.TextUtils;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.autonavi.gbl.common.path.model.TrafficStatus;
import com.autonavi.gbl.common.path.model.ViaPointInfo;
import com.autonavi.gbl.common.path.option.PathInfo;
import com.sgm.navi.service.AdapterConfig;
import com.sgm.navi.service.adapter.layer.LayerAdapter;
import com.sgm.navi.service.adapter.navistatus.NavistatusAdapter;
import com.sgm.navi.service.adapter.route.RouteAdapter;
import com.sgm.navi.service.adapter.setting.SettingAdapter;
import com.sgm.navi.service.define.cruise.CruiseParamEntity;
import com.sgm.navi.service.define.layer.RouteLineLayerParam;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.navi.NaviEtaInfo;
import com.sgm.navi.service.define.navi.NaviInfoEntity;
import com.sgm.navi.service.define.navi.NaviParamEntity;
import com.sgm.navi.service.define.navi.NaviStartType;
import com.sgm.navi.service.define.navi.NaviTmcInfo;
import com.sgm.navi.service.define.navi.NaviViaEntity;
import com.sgm.navi.service.define.navi.SoundInfoEntity;
import com.sgm.navi.service.define.route.RouteParam;
import com.sgm.navi.service.logicpaket.route.RoutePackage;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import lombok.Getter;

/**
 * 导航相关接口适配层
 *
 * @author sgm
 * @version $Revision.*$
 */
public final class NaviAdapter {

    public static final String TAG = "NaviAdapter";
    public static final String NAVI_PKG_NAME =
            Objects.requireNonNull(NaviAdapter.class.getPackage()).getName();
    public static final String NAVI_CLS_NAME = "NaviAdapterApiImpl";
    private INaviApi mNaviApi;
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
    @Getter
    private volatile CountDownLatch mCountDownLatch;

    private ArrayList<NaviInfoEntity> mNaviInfoEntities = new ArrayList<>();
    public static final int THREE_SECONDS = 3 * 1000;

    private NaviAdapter() {
        mNaviApi = (INaviApi) AdapterConfig.getObject(NAVI_PKG_NAME, NAVI_CLS_NAME);
        mLayerAdapter = LayerAdapter.getInstance();
        mNavistatusAdapter = NavistatusAdapter.getInstance();
        mSettingAdapter = SettingAdapter.getInstance();
        mRouteAdapter = RouteAdapter.getInstance();
    }

    /**
     * 初始化导航服务
     */
    public void initNaviService() {
        mNaviApi.initNaviService();
    }

    /**
     * @param key              key
     * @param guidanceObserver guidanceObserver
     */
    public void registerObserver(final String key, final GuidanceObserver guidanceObserver) {
        mNaviApi.registerObserver(key, guidanceObserver);
    }

    /**
     * @param key key
     */
    public void unregisterObserver(final String key) {
        mNaviApi.unregisterObserver(key);
    }

    /**
     * 销毁导航服务
     */
    public void unInitNaviService() {
        mNaviApi.unInitNaviService();
    }

    /**
     * @param isFindRemainPath isFindRemainPath
     * @return long
     */
    public long obtainSAPAInfo(final boolean isFindRemainPath) {
        return mNaviApi.obtainSAPAInfo(isFindRemainPath);
    }

    /**
     * @param pathID pathID
     */
    public void selectMainPathID(final long pathID) {
        mNaviApi.selectMainPathID(pathID);
    }

    /**
     * @param naviStartType naviStartType
     * @return boolean
     */
    public boolean startNavigation(final NaviStartType naviStartType) {
        updateBroadcastParam(mSettingAdapter.getConfigKeyBroadcastMode(), true);
        return mNaviApi.startNavigation(naviStartType);
    }

    /**
     * @return boolean
     */
    public boolean stopNavigation() {
        return mNaviApi.stopNavigation();
    }

    /**
     * @param routeLineLayerParam routeLineLayerParam
     */
    public void updateNaviPath(final RouteLineLayerParam routeLineLayerParam) {
        mNaviApi.updateNaviPath(routeLineLayerParam);
        mLayerAdapter.updateGuideCarStyle(MapType.MAIN_SCREEN_MAIN_MAP);
        mRouteAdapter.sendL2Data(MapType.MAIN_SCREEN_MAIN_MAP);
    }
    public void setNaviPath(final int routeIndex, final RouteLineLayerParam routeLineLayerParam) {
        mNaviApi.setNaviPath(routeIndex, routeLineLayerParam);
    }

    public static NaviAdapter getInstance() {
        return Helper.NAVI_ADAPTER;
    }

    /**
     * @param cruiseParamEntity cruiseParamEntity
     */
    public void setCruiseParam(final CruiseParamEntity cruiseParamEntity) {
        mNaviApi.setCruiseParam(cruiseParamEntity);
    }

    /**
     * @param broadcastType broadcastType
     * @param isDay         isDay
     */
    public void updateBroadcastParam(final int broadcastType, final boolean isDay) {
        final NaviParamEntity naviParamEntity = new NaviParamEntity()
                .setType(NaviConstant.GuideParamType.GUIDE_PARAM_TTS_PLAY)
                .setDay(isDay)
                .setEnableADCode(true)
                .setFatiguedTTS(2);
        if (broadcastType == NaviConstant.BroadcastType.BROADCAST_CONCISE) {
            naviParamEntity.setStyle(4);
            mSettingAdapter.setConfigKeyBroadcastMode(1);
        } else if (broadcastType == NaviConstant.BroadcastType.BROADCAST_MINIMALISM) {
            naviParamEntity.setStyle(6);
            mSettingAdapter.setConfigKeyBroadcastMode(3);
        } else {
            naviParamEntity.setStyle(2);
            mSettingAdapter.setConfigKeyBroadcastMode(2);
        }
        mNaviApi.updateGuideParam(naviParamEntity);
    }

    /**
     * @param segmentIdx segmentIdx
     * @param linkIdx    linkIdx
     */
    public void queryAppointLanesInfo(final int segmentIdx, final int linkIdx) {
        mNaviApi.queryAppointLanesInfo(segmentIdx, linkIdx);
    }

    public void setNaviInfoList(ArrayList<NaviInfoEntity> naviInfoList) {
        mNaviInfoEntities = naviInfoList;
    }

    public ArrayList<NaviInfoEntity> getNaviInfoList() {
        return mNaviInfoEntities;
    }

    public void updateBatteryInfo() {
        mNaviApi.updateBatteryInfo();
    }

    /***
     * 此接口属于动态获取
     * @return 获取途径点信息
     */
    public List<NaviViaEntity> getAllViaPoints(PathInfo pathInfo) {
        return mNaviApi.getAllViaPoints(pathInfo);
    }

    private static final class Helper {
        private static final NaviAdapter NAVI_ADAPTER = new NaviAdapter();
    }

    /**
     * 设置TMC数据
     *
     * @param naviTmcInfo NaviTmcInfo
     */
    public void setTmcData(final NaviTmcInfo naviTmcInfo) {
        mNaviTmcInfo = naviTmcInfo;
    }

    public NaviTmcInfo getTmcData() {
        return mNaviTmcInfo;
    }

    /**
     * 设置导航数据数据
     *
     * @param naviEtaInfo NaviEtaInfo
     */
    public void setNaviEtaInfo(final NaviEtaInfo naviEtaInfo) {
        Logger.i(TAG, "setNaviEtaInfo");
        mNaviEtaInfo = naviEtaInfo;
    }

    /**
     * @param infoEntity infoEntity
     */
    public void setSoundInfoEntity(final SoundInfoEntity infoEntity) {
        Logger.i(TAG, "setSoundInfoEntity");
        mSoundInfoEntity = infoEntity;
    }

    /**
     * 三个参数，地点/路段a、起点b、终点c，a不空其他为空就是查询地点或道路的路况，a为空bc不为空就是查询b到c的路况，
     * ab为空c不空就是查询当前到c的路况，abc都为空就是查询前方路况
     *
     * @param a         String
     * @param b         String
     * @param c         String
     * @param mapTypeId MapTypeId
     * @return int 交通状态回调 -1：无数据 0:未知状态 1:通畅 2:缓慢 3:拥堵 4:严重拥堵 5:极度通畅
     */
    public int getTmcStatus(final String a, final String b, final String c,
                            final MapType mapTypeId) {
        Logger.i(TAG, "getTmcStatus a:" , a , " b:" , b , " c:" , c , " mapTypeId:" ,
                mapTypeId);
        final PathInfo pathInfo = (PathInfo)
                Objects.requireNonNull(mRouteAdapter.getCurrentPath(mapTypeId)).getMPathInfo();
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

    /**
     * abc都为空就是查询前方路况
     *
     * @return tts
     */
    public String getFrontTmcStatus() {
        try {
            mCountDownLatch = new CountDownLatch(1);
            // 主动获取前方的路况信息
            mNaviApi.playTRManualExt(0);
            // 延迟3秒后看是否获取到了tts数据
            ThreadManager.getInstance().asyncDelay(mRunnable, THREE_SECONDS, TimeUnit.MILLISECONDS);
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

    private final Runnable mRunnable = new Runnable() {
        @Override
        public void run() {
            if (mCountDownLatch != null) {
                final long count = mCountDownLatch.getCount();
                if (count != 0) {
                    mSoundInfoEntity = null;
                    mCountDownLatch.countDown();
                }
            }
        }
    };

    /**
     * 获取途经点对应的路段索引
     *
     * @param viaName  途经点名称
     * @param pathInfo 路径信息
     * @return 途经点对应的路段索引
     */
    private short getViaSegIdx(final String viaName, final PathInfo pathInfo) {
        //特殊情况需要判断下传入的位置是否是目的地 因为获取的途经点里面没有包含目的地
        final List<RouteParam> allPoiParamList = RoutePackage.getInstance().
                getAllPoiParamList(MapType.MAIN_SCREEN_MAIN_MAP);
        if (!ConvertUtils.isEmpty(allPoiParamList)) {
            // 先判断是否是目的地
            final String endName = allPoiParamList.get(allPoiParamList.size() - 1).getName();
            if (viaName.equals(endName)) {
                return -2;
            }
        }
        final List<ViaPointInfo> viaPointInfoList = pathInfo.getViaPointInfo();
        for (ViaPointInfo viaPointInfo : viaPointInfoList) {
            if (viaName.equals(viaPointInfo.poiName)) {
                return viaPointInfo.segmentIdx;
            }
        }
        return -1;
    }

    /**
     * 获取对应路径的路况信息
     *
     * @param naviLightBarInfoList 路况信息列表
     * @param pathInfo             路径信息
     * @return 路径上的路况信息
     */
    public NaviTmcInfo.NaviLightBarInfo getCurrentLightBarInfo(
            final ArrayList<NaviTmcInfo.NaviLightBarInfo> naviLightBarInfoList,
            final PathInfo pathInfo) {
        for (NaviTmcInfo.NaviLightBarInfo naviLightBarInfo : naviLightBarInfoList) {
            if (naviLightBarInfo.getPathID() == pathInfo.getPathID()) {
                return naviLightBarInfo;
            }
        }
        return null;
    }

    /**
     * 获取当点周边的交通情况
     *
     * @param viaName  途经点名称
     * @param pathInfo 路径信息
     * @return 交通状态
     */
    private int getPointTmcStatus(final String viaName, final PathInfo pathInfo) {
        final short viaSegmentIdx = getViaSegIdx(viaName, pathInfo);
        Logger.i(TAG, "getPointTmcStatus viaSegmentIdx:" , viaSegmentIdx);
        if (viaSegmentIdx < 0 && viaSegmentIdx != -2) {
            return -1;
        }
        if (mNaviTmcInfo != null) {
            final NaviTmcInfo.NaviLightBarInfo currentLightBarInfo = getCurrentLightBarInfo(
                    mNaviTmcInfo.getLightBarInfo(), pathInfo);
            // 获取到途经点的路况信息并返回
            if (currentLightBarInfo != null) {
                final ArrayList<NaviTmcInfo.NaviLightBarItem> naviLightBarItems = currentLightBarInfo.
                        getItemList();
                if (ConvertUtils.isEmpty(naviLightBarItems)) {
                    Logger.i(TAG, "getPointTmcStatus naviLightBarItems is null");
                    return -1;
                }
                Logger.i(TAG, "getPointTmcStatus " + "viaSegmentIdx = " + viaSegmentIdx +
                        "naviLightBarItems:" +
                        naviLightBarItems.toString());
                // 搜索的是目的地
                if (viaSegmentIdx == -2) {
                    // 取最后一段路况信息，涵盖了目的地
                    final NaviTmcInfo.NaviLightBarItem endNaviLightBarItem =
                            naviLightBarItems.get(naviLightBarItems.size() - 1);
                    if (endNaviLightBarItem.getStatusFlag() == 0x00) {
                        return endNaviLightBarItem.getStatus();
                    } else {
                        return getFineStatus(endNaviLightBarItem.getFineStatus());
                    }
                }
                for (NaviTmcInfo.NaviLightBarItem naviLightBarItem : naviLightBarItems) {
                    if (viaSegmentIdx >= naviLightBarItem.getStartSegmentIdx() &&
                            viaSegmentIdx <= naviLightBarItem.getEndSegmentIdx()) {
                        if (naviLightBarItem.getStatusFlag() == 0x00) {
                            return naviLightBarItem.getStatus();
                        } else {
                            return getFineStatus(naviLightBarItem.getFineStatus());
                        }
                    }
                }
            }
        }
        return -1;
    }

    /**
     * 获取一段路线的交通状态 比如从A点到B点的交通状态
     *
     * @param a        地点a
     * @param b        地点b
     * @param pathInfo pathInfo
     * @return int
     */
    private int getDistanceTmcStatus(final String a, final String b, final PathInfo pathInfo) {
        final short viaASegmentIdx = getViaSegIdx(a, pathInfo);
        final short viaBSegmentIdx = getViaSegIdx(b, pathInfo);

        Logger.i(TAG, "getPointTmcStatus viaASegmentIdx:" + viaASegmentIdx +
                ",viaBSegmentIdx:" + viaBSegmentIdx);
        // 这种情况下只有b点可能是目的地
        if (viaASegmentIdx < 0 || (viaBSegmentIdx < 0 && viaBSegmentIdx != -2)) {
            return -1;
        }
        return getDistanceTmcStatus(viaASegmentIdx, viaBSegmentIdx, pathInfo);
    }

    /**
     * 获取当前到途经点的交通状态
     *
     * @param viaName  途经点名称
     * @param pathInfo pathInfo
     * @return int
     */
    private int getToViaPointTmcStatus(final String viaName, final PathInfo pathInfo) {
        final short viaSegmentIdx = getViaSegIdx(viaName, pathInfo);
        int currentSegmentIdx = -1;
        if (mNaviEtaInfo != null) {
            currentSegmentIdx = mNaviEtaInfo.curSegIdx;
        }
        Logger.i(TAG, "getPointTmcStatus viaSegmentIdx: ", viaSegmentIdx, "; currentSegIdx: ", currentSegmentIdx);
        if ((viaSegmentIdx < 0 && viaSegmentIdx != -2) || currentSegmentIdx < 0) {
            return -1;
        }
        return getDistanceTmcStatus((short) currentSegmentIdx, viaSegmentIdx, pathInfo);
    }

    /**
     * @param startSegmentIdx 起始点路段索引
     * @param endSegmentIdx   终点路段索引
     * @param pathInfo        路径信息
     * @return 两点之间的路况信息
     */
    private int getDistanceTmcStatus(final short startSegmentIdx, final short endSegmentIdx,
                                     final PathInfo pathInfo) {
        if (mNaviTmcInfo != null) {
            final NaviTmcInfo.NaviLightBarInfo currentLightBarInfo = getCurrentLightBarInfo(
                    mNaviTmcInfo.getLightBarInfo(), pathInfo);
            // 获取到途经路段的路况信息并返回
            if (currentLightBarInfo != null) {
                final ArrayList<NaviTmcInfo.NaviLightBarItem> naviLightBarItems =
                        currentLightBarInfo.getItemList();
                if (ConvertUtils.isEmpty(naviLightBarItems)) {
                    Logger.i(TAG, "getPointTmcStatus naviLightBarItems is null");
                    return -1;
                }
                Logger.i(TAG, "getPointTmcStatus startSegmentIdx = " + startSegmentIdx +
                        " endSegmentIdx = " + endSegmentIdx + "naviLightBarItems:" +
                        naviLightBarItems.toString());
                // a点路况解析
                for (NaviTmcInfo.NaviLightBarItem naviLightBarItem : naviLightBarItems) {
                    if (endSegmentIdx != -2) {
                        if ((startSegmentIdx < naviLightBarItem.getEndSegmentIdx() &&
                                startSegmentIdx >= naviLightBarItem.getStartSegmentIdx()) ||
                                (endSegmentIdx < naviLightBarItem.getEndSegmentIdx() &&
                                        endSegmentIdx >= naviLightBarItem.getStartSegmentIdx())) {
                            if (naviLightBarItem.getStatusFlag() == 0x00) {
                                return naviLightBarItem.getStatus();
                            } else {
                                return getFineStatus(naviLightBarItem.getFineStatus());
                            }
                        }
                    }
                    // b点目的地的情况
                    final NaviTmcInfo.NaviLightBarItem endNaviLightBarItem =
                            naviLightBarItems.get(naviLightBarItems.size() - 1);
                    if (endNaviLightBarItem.getStatusFlag() == 0x00) {
                        return endNaviLightBarItem.getStatus();
                    } else {
                        return getFineStatus(endNaviLightBarItem.getFineStatus());
                    }
                }
            }
        }
        return -1;
    }

    /**
     * 精细化交通状态转换
     *
     * @param fineStatus 精细化交通状态
     * @return int交通状态
     */
    private int getFineStatus(final int fineStatus) {
        // 畅通
        if (fineStatus >= 100 && fineStatus < 200) {
            return TrafficStatus.TrafficStatusOpen;
            // 缓行
        } else if (fineStatus >= 200 && fineStatus < 300) {
            return TrafficStatus.TrafficStatusSlow;
            // 拥堵
        } else if (fineStatus >= 300 && fineStatus < 400) {
            return TrafficStatus.TrafficStatusJam;
        } else {
            return 0;
        }
    }

    public void pauseNavi() {
        mNaviApi.pauseNavi();
    }

    public void resumeNavi() {
        mNaviApi.resumeNavi();
    }

    public void setSimulationSpeed(int simulationSpeed) {
        mNaviApi.setSimulationSpeed(simulationSpeed);
    }

    /**
     * 更新引导路线数据
     * @param pathInfoList 路线数据
     * @param selectIndex 选中下标
     */
    public boolean updatePathInfo(MapType mapTypeId, ArrayList<?> pathInfoList, int selectIndex) {
        Logger.i(TAG, "updatePathInfo pathInfoList.size = " +
                (!ConvertUtils.isEmpty(pathInfoList) ? pathInfoList.size() : 0) +
                " selectIndex = " + selectIndex + " mapTypeId = " + mapTypeId);
        if (null != mLayerAdapter) {
            return mLayerAdapter.updatePathInfo(mapTypeId, pathInfoList, selectIndex);
        }
        return false;
    }
}

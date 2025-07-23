package com.sgm.navi.hmi.navi;


import android.text.TextUtils;

import com.android.utils.ConvertUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.hmi.utils.StringUtils;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.ui.navi.ChargeTipEntity;
import com.sgm.navi.scene.ui.navi.SceneNaviChargeBtnType;
import com.sgm.navi.scene.ui.navi.hangingcard.CardManager;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.search.cloudByPatac.rep.BaseRep;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.calibration.PowerType;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.navi.FyElecVehicleETAInfo;
import com.sgm.navi.service.define.navi.NaviEtaInfo;
import com.sgm.navi.service.define.navi.NaviViaEntity;
import com.sgm.navi.service.define.position.LocInfoBean;
import com.sgm.navi.service.define.route.Coord3DDouble;
import com.sgm.navi.service.define.route.EvRangeOnRouteInfo;
import com.sgm.navi.service.define.route.RouteAlterChargeStationInfo;
import com.sgm.navi.service.define.route.RouteAlterChargeStationParam;
import com.sgm.navi.service.define.search.ChargeInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.search.ReservationInfo;
import com.sgm.navi.service.define.search.SearchResultEntity;
import com.sgm.navi.service.define.user.account.AccessTokenParam;
import com.sgm.navi.service.define.utils.BevPowerCarUtils;
import com.sgm.navi.service.logicpaket.calibration.CalibrationPackage;
import com.sgm.navi.service.logicpaket.layer.LayerPackage;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.service.logicpaket.position.PositionPackage;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.service.logicpaket.setting.SettingPackage;
import com.sgm.navi.service.logicpaket.speech.SpeechPackage;
import com.sgm.navi.service.logicpaket.user.account.AccountPackage;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.time.Duration;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.ArrayList;
import java.util.List;

public class ChargeTipManager {
    private static final String TAG = MapDefaultFinalTag.NAVI_SCENE_CHARGE_TIP_MANAGER;
    private NaviGuidanceViewModel mViewModel;
    private final SpeechPackage mSpeechPackage;
    private final SearchPackage mSearchPackage;
    private final RoutePackage mRoutePackage;
    private final SettingPackage mSettingPackage;
    private final CalibrationPackage mCalibrationPackage;
    private final NaviPackage mNaviPackage;
    //所有途经点集合
    private List<NaviViaEntity> mNaviViaEntityList;
    //补能规划是否已打开
    private boolean mIsChargingPlanOpen;
    //当前途经点里程进入10分钟以内后是否已经进行消息条件判断
    private boolean mIsCurViaChecked;
    //当前途经点
    private PoiInfoEntity mPoiInfoEntity;
    //当前途经点详情静默搜索ID
    private int mViaDetailSearchId;
    //是否正在搜索
    private boolean mIsViaDetailSearching;
    //备选充电站请求ID
    private long mAlternativeChargeStationId;
    //当前途经点充电站空闲充电桩数量
    private int curViaFree = 0;
    //当前途经点充电站总充电桩数量
    private int curViaTotal = 0;
    //沿途充电站搜索ID
    private int mRouteChargeSearchId;
    //剩余里程内充电站数量少是否已提醒
    private boolean mIsChargeNumChecked;
    //错过充电站请求备选充电站ID
    private long mPassAlternativeId;
    //提醒消息类型
    private TipType mTipType;
    //电量预警是否已提示
    private boolean isLowPowerNotified;
    //查询预约充电桩ID
    private int mReservationTaskId;
    //检测频率控制
    private int mIntervalCount;
    //剩余电量低打开补能规划是否已提醒
    private boolean mIsPowerLowOpenSupplyChecked;
    private RouteAlterChargeStationInfo mRouteAlterChargeStationInfo;

    private boolean mTestIsMocking = false;

    public ChargeTipManager(final NaviGuidanceViewModel naviGuidanceModel) {
        this.mViewModel = naviGuidanceModel;
        mSpeechPackage = SpeechPackage.getInstance();
        mSearchPackage = SearchPackage.getInstance();
        mRoutePackage = RoutePackage.getInstance();
        mSettingPackage = SettingPackage.getInstance();
        mCalibrationPackage = CalibrationPackage.getInstance();
        mNaviPackage = NaviPackage.getInstance();
    }

    /***
     * 释放资源
     */
    public void unInit() {
        mViewModel = null;
        mTestIsMocking = false;
        Logger.i(TAG, "unInit success!");
    }

    /**
     * 途经点信息更新
     *
     * @param list
     */
    public void updateViaList(final List<NaviViaEntity> list) {
        if (mNaviViaEntityList == null) {
            mNaviViaEntityList = new ArrayList<>();
        }
        mNaviViaEntityList.clear();
        if (!ConvertUtils.isEmpty(list)) {
            mNaviViaEntityList.addAll(list);
        }
        mIsCurViaChecked = false;
    }

    /***
     * 设置下一个即将通过的充电站
     * @param naviETAInfo
     */
    public void setNextViaChargeStation(final NaviEtaInfo naviETAInfo) {
        if (mCalibrationPackage.powerType() != PowerType.E_VEHICLE_ENERGY_ELECTRIC) {
            //不是纯电动汽车
            return;
        }
        mIntervalCount++;
        if (mIntervalCount < 60) {
            return;
        }
        mIntervalCount = 0;
        if (!mSettingPackage.getPushMessage()) {
            //推送消息开关未打开
            if (Logger.openLog) {
                Logger.i(TAG, "PushMessage is not open");
            }
            return;
        }
        mIsChargingPlanOpen = SettingPackage.getInstance().getChargingPlan();//补能规划开关
        boolean isCurClose = SettingPackage.getInstance().getCurCloseChargingPlan();//是否当前上电内关闭的补能开关
        //boolean isRouteTips = mRoutePackage.getRouteTips();//路径规划页是否已提醒电量低
        if (Logger.openLog) {
            Logger.i(TAG, "mIsChargeNum:", mIsChargeNumChecked, " mIsPowerLowChecked:", mIsPowerLowOpenSupplyChecked
                    , " PlanOpen:", mIsChargingPlanOpen, " isCurClose:", isCurClose, " isLowPower:", isLowPowerNotified,
                    " mIsCurViaChecked:", mIsCurViaChecked, " mIsViaDetailSearching:", mIsViaDetailSearching);
        }
        checkViaChargeStationState(naviETAInfo);
        if (!mIsChargeNumChecked) {
            checkChargeStationNum();
        }
        if (!mIsChargingPlanOpen && !isCurClose) {
            if (mRoutePackage.isRouteTips()) {
                if (Logger.openLog) {
                    Logger.i(TAG, "isRouteTips is true");
                }
            } else {
                if (!mIsPowerLowOpenSupplyChecked && isUnreachableOrLow()) {
                    mIsPowerLowOpenSupplyChecked = true;
                    notifyPowerLowOpenSupply();
                }
            }
        }

        if (!isLowPowerNotified && isRemainLow()) {
            //续航里程不足50公里
            isLowPowerNotified = true;
            lowPowerGoCharge();
        }

        //有预约时查询预约情况
        if (!ConvertUtils.isEmpty(mSearchPackage.getReservationPreNum())) {
            queryReservation();
        }
    }

    /***
     *电动车ETA透出---频率1分钟1次，需要实车测试
     * 注意：该接口仅在在线模式下有效，离线模式不支持
     * @param infos
     */
    public void onUpdateElectVehicleETAInfo(final List<FyElecVehicleETAInfo> infos) {
    }

    public void onSilentSearchResult(int taskId, int errorCode, String message, SearchResultEntity searchResultEntity) {
        viaDetailSearchResult(taskId, errorCode, message, searchResultEntity);
        routeChargeSearchResult(taskId, errorCode, message, searchResultEntity);
    }

    public void onNetSearchResult(int taskId, String searchKey, BaseRep result) {
        queryReservationResult(taskId, searchKey, result);
    }

    public void onRouteAlterChargeStationInfo(RouteAlterChargeStationParam routeAlterChargeStationParam) {
        if (routeAlterChargeStationParam.getMRequestId() == mAlternativeChargeStationId) {
            checkAlternativeReachable(routeAlterChargeStationParam.getMRouteAlterChargeStationInfos(), mTipType);
        }
        if (routeAlterChargeStationParam.getMRequestId() == mPassAlternativeId) {
            checkAlternativeReachable(routeAlterChargeStationParam.getMRouteAlterChargeStationInfos(), TipType.Pass);
        }
    }

    public void onUpdateChargeStationPass(long viaIndex) {
        // 将要通过的充电桩回调
        Logger.i(TAG, "viaIndex：", viaIndex, " mNaviViaEntityList:", mNaviViaEntityList == null ? "null" : mNaviViaEntityList.size());
        if (!ConvertUtils.isEmpty(mNaviViaEntityList) && mNaviViaEntityList.size() > viaIndex) {
            NaviViaEntity viaEntity = mNaviViaEntityList.get((int) viaIndex);
            Logger.i(TAG, "viaEntity: ", viaEntity);
            if (viaEntity == null) {
                return;
            }
            ThreadManager.getInstance().asyncDelay(new Runnable() {
                @Override
                public void run() {
                    final LocInfoBean locInfoBean = PositionPackage.getInstance().getLastCarLocation();
                    GeoPoint startPoint = new GeoPoint(locInfoBean.getLongitude(), locInfoBean.getLatitude(), locInfoBean.getAltitude());
                    double distance = LayerPackage.getInstance().calcStraightDistance(startPoint, viaEntity.getRealPos());
                    Logger.i(TAG, "onUpdateChargeStationPass distance: ", distance);
                    if (distance > 1000) {
                        mPassAlternativeId = mRoutePackage.requestRouteAlternativeChargeStation(MapType.MAIN_SCREEN_MAIN_MAP, viaEntity.getPid());
                    }
                }
            }, 5 * 60);
        }
    }


    /**
     * 检测当前途经点充电站状态：拥挤、未营业
     */
    private void checkViaChargeStationState(NaviEtaInfo naviETAInfo) {
        if (naviETAInfo == null) {
            return;
        }
        NaviEtaInfo.NaviTimeAndDist viaChargeStation;
        if (mIsChargingPlanOpen) {
            //补能规划打开后使用补能充电桩
            if (ConvertUtils.isEmpty(naviETAInfo.ChargeStationRemain)) {
                return;
            }
            viaChargeStation = naviETAInfo.ChargeStationRemain.get(0);
        } else {
            //补能规划未打开使用手动添加的途经点充电站
            if (ConvertUtils.isEmpty(naviETAInfo.viaRemain)) {
                return;
            }
            viaChargeStation = naviETAInfo.viaRemain.get(0);
        }
        if (viaChargeStation == null) {
            return;
        }
        if (mIsCurViaChecked) {
            return;
        }
        if (mIsViaDetailSearching) {
            return;
        }
        if (ConvertUtils.isEmpty(mNaviViaEntityList)) {
            return;
        }
        NaviViaEntity viaEntity = mNaviViaEntityList.get(0);
        if (Logger.openLog) {
            Logger.i(TAG, "viaEntity: ", viaEntity);
        }
        if (ConvertUtils.isEmpty(viaEntity)) {
            return;
        }
        boolean needQuery = false;
        mTipType = TipType.Invalid;
        if (mIsChargingPlanOpen && getRemainDistance() < viaChargeStation.dist) {
            mTipType = TipType.Unreachable;
            needQuery = true;
        } else if (viaChargeStation.time < 10 * 60) {
            needQuery = true;
        }
        Logger.i(TAG, "needQuery: ", needQuery, " mTipType:" + mTipType);
        if (needQuery) {
            mIsViaDetailSearching = true;
            mViaDetailSearchId = mSearchPackage.poiIdSearch(viaEntity.getPid(), true);
            Logger.i(TAG, "mViaDetailSearchId: ", mViaDetailSearchId);
        }
    }

    private void viaDetailSearchResult(int taskId, int errorCode, String message, SearchResultEntity searchResultEntity) {
        if (mViaDetailSearchId != taskId) {
            return;
        }
        Logger.i(TAG, "errorCode:", errorCode, "message:", message, " 搜索结果:", searchResultEntity);
        mIsViaDetailSearching = false;
        if (ConvertUtils.isEmpty(searchResultEntity)) {
            return;
        }
        List<PoiInfoEntity> poiInfos = searchResultEntity.getPoiList();
        if (ConvertUtils.isEmpty(poiInfos)) {
            return;
        }
        mIsCurViaChecked = true;
        PoiInfoEntity poiInfo = poiInfos.get(0);
        if (!CardManager.getInstance().judgePoiIsChargeStation(poiInfo)) {
            return;
        }
        List<ChargeInfo> chargeInfoList = poiInfo.getChargeInfoList();
        if (ConvertUtils.isEmpty(chargeInfoList)) {
            return;
        }
        mPoiInfoEntity = poiInfo;
        curViaFree = 0;
        curViaTotal = 0;
        for (ChargeInfo chargeInfo : chargeInfoList) {
            if (chargeInfo != null) {
                curViaFree = curViaFree + chargeInfo.getFast_free() + chargeInfo.getSlow_free();
                curViaTotal = curViaTotal + chargeInfo.getFast_total() + chargeInfo.getSlow_total();
            }
        }
        Logger.i(TAG, "curViaFree:", curViaFree, " curViaTotal:", curViaTotal, " mIsChargingPlanOpen:", mIsChargingPlanOpen);
        if (mIsChargingPlanOpen) {
            //补能规划打开：检测能否合理规划替补充电站
            if (mTipType != TipType.Unreachable) {
                if (!isCurrentTimeInRange(poiInfo.getBusinessTime())) {
                    mTipType = TipType.Closed;
                } else if (curViaFree < 2) {
                    mTipType = TipType.Congestion;
                }
            }
            mAlternativeChargeStationId = mRoutePackage.requestRouteAlternativeChargeStation(MapType.MAIN_SCREEN_MAIN_MAP, poiInfo.getPid());
        } else {
            if (!isCurrentTimeInRange(poiInfo.getBusinessTime())) {
                chargeStationCloseSearchNew(poiInfo);
            } else if (curViaFree < 2) {
                chargeStationCongestionSearchNew(poiInfo);
            }
        }
    }

    /**
     * 检测剩余充电站数量是否较少
     */
    private void checkChargeStationNum() {
        if (isRemainLow() && isUnreachableOrLess20()) {
            mRouteChargeSearchId = mSearchPackage.enRouteKeywordSearch("充电站", true);
        }
    }

    private void routeChargeSearchResult(int taskId, int errorCode, String message, SearchResultEntity searchResultEntity) {
        if (mRouteChargeSearchId != taskId) {
            return;
        }
        if (ConvertUtils.isEmpty(searchResultEntity)) {
            Logger.i(TAG, "errorCode:", errorCode, "message:" + message);
            return;
        }
        List<PoiInfoEntity> poiInfos = searchResultEntity.getPoiList();
        Logger.i(TAG, "poiInfos:", (poiInfos == null ? "null" : poiInfos.size()));
        if (ConvertUtils.isEmpty(poiInfos) || poiInfos.size() < 2) {
            mIsChargeNumChecked = true;
            chargeStationFewGoCharge();
        }
    }

    /**
     * 预约详情查询
     *
     * @param taskId
     * @param searchKey
     * @param result
     */
    private void queryReservationResult(int taskId, String searchKey, BaseRep result) {
        if (mReservationTaskId != taskId) {
            return;
        }
        if (result == null) {
            Logger.i(TAG, "result == null");
            return;
        }
        Logger.i(TAG, "搜索结果 searchKey:", searchKey, " ResultCode:", result.getResultCode(), " Message:", result.getMessage());
        if (AutoMapConstant.NetSearchKey.SUCCESS_CODE.equals(result.getResultCode())) {
            ArrayList<ReservationInfo> preList = new ArrayList<>();
            // 回调出的数据转换List
            try {
                JSONObject jsonObject = new JSONObject(GsonUtils.toJson(result.getDataSet()));
                //Logger.i(TAG, "jsonObject:", jsonObject.toString());
                JSONArray jsonArray = jsonObject.getJSONArray("resultList");
                String userId = AccountPackage.getInstance().getUserId();
                for (int i = 0; i < jsonArray.length(); i++) {
                    ReservationInfo reservationInfo = GsonUtils.fromJson(String.valueOf(jsonArray.get(i)), ReservationInfo.class);
                    if (reservationInfo.getmUserId().equals(userId) && reservationInfo.getmStatus() == 1) {
                        preList.add(reservationInfo);
                    }
                }
                if (!preList.isEmpty()) {
                    ReservationInfo info = preList.get(0);//2021-12-28 10:59:53",
                    if (info == null || ConvertUtils.isEmpty(info.getmCreateTime())) {
                        Logger.i(TAG, "info == null || ConvertUtils.isEmpty(info.getmCreateTime())");
                        return;
                    }
                    if (!StringUtils.isDatetimeByRegex(info.getmCreateTime())) {
                        Logger.i(TAG, "getmCreateTime:", info.getmCreateTime());
                        return;
                    }
                    DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
                    LocalTime createTime = LocalTime.parse(info.getmCreateTime(), formatter);
                    LocalTime now = LocalTime.now();
                    if (Logger.openLog) {
                        Logger.i(TAG, "createTime:", info.getmCreateTime(), " now:", formatter.format(now));
                    }
                    if (createTime == null) {
                        return;
                    }
                    if (now.isAfter(createTime)) {
                        return;
                    }
                    if (ConvertUtils.isEmpty(info.getmLng()) || ConvertUtils.isEmpty(info.getmLat())) {
                        return;
                    }
                    double lon = 0;
                    double lat = 0;
                    try {
                        lon = Double.parseDouble(info.getmLng());
                        lat = Double.parseDouble(info.getmLat());
                        return;
                    } catch (NumberFormatException e) {
                        e.printStackTrace();
                    }
                    if (lon == 0 || lat == 0) {
                        Logger.i(TAG, "lon:", lon, " lat:", lat);
                        return;
                    }
                    long min = Duration.between(now, createTime).toMinutes();
                    final LocInfoBean locInfoBean = PositionPackage.getInstance().getLastCarLocation();
                    if (locInfoBean == null) {
                        return;
                    }
                    GeoPoint startPoint = new GeoPoint(locInfoBean.getLongitude(), locInfoBean.getLatitude(), locInfoBean.getAltitude());
                    GeoPoint endPoint = new GeoPoint(lon, lat);
                    mSearchPackage.getTravelTimeFutureIncludeChargeLeft(startPoint, endPoint).thenAccept(etaInfo -> {
                        if (etaInfo == null) {
                            Logger.i(TAG, "etaInfo == null");
                            return;
                        }
                        Logger.i(TAG, "getTime():", etaInfo.getTime(), "min:", min);
                        if (min - etaInfo.getTime() / 60 < 5 * 60) {
                            chargeStationTimeOver((int) min);
                        }
                    }).exceptionally(throwable -> {
                        Logger.i(TAG, "获取两点之间路线距离 error:", throwable.getMessage());
                        return null;
                    });
                }
            } catch (JSONException e) {
                throw new RuntimeException(e);
            }
        } else {
            Logger.e(MapDefaultFinalTag.SEARCH_HMI_TAG, "onQueryEquipmentResult error");
        }
    }

    /**
     * 是否剩余续航低 小于50公里
     */
    private boolean isRemainLow() {
        int remain = getRemainDistance();
        Logger.i(TAG, "remain:", remain);
        return remain < 50 * 1000;
    }

    /**
     * 剩余续航里程 米
     */
    private int getRemainDistance() {
        return (int) (BevPowerCarUtils.getInstance().initlialHVBattenergy * BevPowerCarUtils.getInstance().batterToDistance);
    }

    /**
     * 目的地不可到达或到达目的地后剩余里程低于阈值
     *
     * @return
     */
    private boolean isUnreachableOrLow() {
        if (mRoutePackage.getSelectRouteIndex() == null) {
            return false;
        }
        final ArrayList<EvRangeOnRouteInfo> evRangeOnRouteInfos = mRoutePackage.getEvRangeOnRouteInfos();
        Integer routeIndex = mRoutePackage.getSelectRouteIndex().get(MapType.MAIN_SCREEN_MAIN_MAP);
        Logger.i(TAG, "routeIndex:", routeIndex, " evRangeOnRouteInfos:", (evRangeOnRouteInfos == null ? "null" : evRangeOnRouteInfos.size()));
        if (!ConvertUtils.isEmpty(evRangeOnRouteInfos) && routeIndex != null) {
            if (evRangeOnRouteInfos.size() > routeIndex && routeIndex >= 0) {
                EvRangeOnRouteInfo rangeOnRouteInfo = evRangeOnRouteInfos.get(routeIndex);
                Logger.i(TAG, "rangeOnRouteInfo:", rangeOnRouteInfo);
                if (rangeOnRouteInfo != null) {
                    if (!rangeOnRouteInfo.isMCanArrived()) {
                        return true;
                    } else {
                        return rangeOnRouteInfo.getMRemainCapacity() < 20;
                    }
                }
            }
        }
        return false;
    }

    /**
     * 不可到达或到达目的地后剩余里程低于20公里
     *
     * @return
     */
    private boolean isUnreachableOrLess20() {
        if (mRoutePackage.getSelectRouteIndex() == null) {
            return false;
        }
        final ArrayList<EvRangeOnRouteInfo> evRangeOnRouteInfos = mRoutePackage.getEvRangeOnRouteInfos();
        Integer routeIndex = mRoutePackage.getSelectRouteIndex().get(MapType.MAIN_SCREEN_MAIN_MAP);
        Logger.i(TAG, "routeIndex:", routeIndex, " evRangeOnRouteInfos:", (evRangeOnRouteInfos == null ? "null" : evRangeOnRouteInfos.size()));
        if (!ConvertUtils.isEmpty(evRangeOnRouteInfos) && routeIndex != null) {
            if (evRangeOnRouteInfos.size() > routeIndex && routeIndex >= 0) {
                EvRangeOnRouteInfo rangeOnRouteInfo = evRangeOnRouteInfos.get(routeIndex);
                Logger.i(TAG, "rangeOnRouteInfo:", rangeOnRouteInfo);
                if (rangeOnRouteInfo != null) {
                    if (!rangeOnRouteInfo.isMCanArrived()) {
                        return true;
                    } else {
                        return rangeOnRouteInfo.getMRemainCapacity() < 20;
                    }
                }
            }
        }
        return false;
    }

    /**
     * 检测备选充电站是否可合理规划路线
     *
     * @param infoArrayList
     * @return
     */
    private void checkAlternativeReachable(ArrayList<RouteAlterChargeStationInfo> infoArrayList, TipType type) {
        if (ConvertUtils.isEmpty(infoArrayList)) {
            Logger.i(TAG, "isEmpty(infoArrayList) type:", type);
            searchNewChargeStationByType(type);
            return;
        }
        final RouteAlterChargeStationInfo info = infoArrayList.get(0);
        if (ConvertUtils.isEmpty(info)) {
            Logger.i(TAG, "isEmpty(info) type:", type);
            searchNewChargeStationByType(type);
            return;
        }
        if (info.getMRemainingPercent() < 20) {
            Logger.i(TAG, "type:" + type, " RemainingPercent:", info.getMRemainingPercent());
            searchNewChargeStationByType(type);
            return;
        }
        Coord3DDouble coord3DDouble = info.getMPos();
        if (coord3DDouble == null) {
            Logger.i(TAG, "coord3DDouble == null type:", type);
            return;
        }
        final LocInfoBean locInfoBean = PositionPackage.getInstance().getLastCarLocation();
        if (locInfoBean == null) {
            Logger.i(TAG, "locInfoBean == null type:", type);
            return;
        }
        GeoPoint startPoint = new GeoPoint(locInfoBean.getLongitude(), locInfoBean.getLatitude(), locInfoBean.getAltitude());
        GeoPoint endPoint = new GeoPoint(coord3DDouble.getLon(), coord3DDouble.getLat(), coord3DDouble.getZ());
        mSearchPackage.getTravelTimeFutureIncludeChargeLeft(startPoint, endPoint).thenAccept(etaInfo -> {
            Logger.i(TAG, "checkAlternativeReachable etaInfo:", etaInfo + " type:", type);
            if (etaInfo == null) {
                return;
            }
            if (etaInfo.getDistance() < 5 * 1000) {
                if (type == TipType.Congestion) {
                    chargeStationCongestionUpdate(info);
                } else if (type == TipType.Pass) {
                    if (SearchPackage.getInstance().isAlongWaySearch()) {
                        addViaList(info);
                        chargeStationPassUpdate(mPoiInfoEntity);//TODO 回调中使用
                    }
                } else if (type == TipType.Closed) {
                    if (SearchPackage.getInstance().isAlongWaySearch()) {
                        addViaList(info);
                        chargeStationClosedUpdate(mPoiInfoEntity);//TODO 回调中使用
                    }
                } else if (type == TipType.Unreachable) {
                    if (SearchPackage.getInstance().isAlongWaySearch()) {
                        addViaList(info);
                        chargeStationUnreachableUpdate(mPoiInfoEntity);//TODO 回调中使用
                    }
                }
            } else {
                searchNewChargeStationByType(type);
            }
        }).exceptionally(throwable -> {
            Logger.i(TAG, "checkAlternativeReachable 获取两点之间路线距离 error:", throwable.getMessage());
            searchNewChargeStationByType(type);
            return null;
        });
    }

    /**
     * 根据提醒类型弹出消息查找新站
     *
     * @param type
     */
    private void searchNewChargeStationByType(TipType type) {
        if (type == TipType.Congestion) {
            chargeStationCongestionSearchNew(mPoiInfoEntity);
        } else if (type == TipType.Pass) {
            chargeStationPassSearchNew(mPoiInfoEntity);
        } else if (type == TipType.Closed) {
            chargeStationCloseSearchNew(mPoiInfoEntity);
        } else if (type == TipType.Unreachable) {
            chargeStationUnreachableSearchNew(mPoiInfoEntity);
        }
    }

    /**
     * 提醒途经点充电站拥挤,查找新站
     */
    private void chargeStationCongestionSearchNew(PoiInfoEntity poiInfo) {
        if (poiInfo == null) {
            Logger.i(TAG, "poiInfo == null");
            return;
        }
        final ChargeTipEntity entity = new ChargeTipEntity();
        entity.setTitle(String.format(getStringRes(R.string.tip_msg_2), poiInfo.getName()) + curViaFree + "/" + curViaTotal);
        entity.setAction(getStringRes(R.string.msg_action_search_new));
        entity.setType(SceneNaviChargeBtnType.SEARCH_NEW_STATION);
        String tts = String.format(getStringRes(R.string.tip_msg_2_tts), poiInfo.getName());
        entity.setTtsContent(tts);
        notifyUi(entity,false);
        mNaviPackage.playChargeTips(tts, SceneNaviChargeBtnType.SEARCH_NEW_STATION);
    }

    /**
     * 提醒途经点充电站未营业,查找新站
     */
    private void chargeStationCloseSearchNew(PoiInfoEntity poiInfo) {
        if (poiInfo == null) {
            Logger.i(TAG, "poiInfo == null");
            return;
        }
        final ChargeTipEntity entity = new ChargeTipEntity();
        entity.setTitle(String.format(getStringRes(R.string.tip_msg_10), poiInfo.getName()));
        entity.setAction(getStringRes(R.string.msg_action_search_new));
        entity.setType(SceneNaviChargeBtnType.SEARCH_NEW_STATION);
        String tts = String.format(getStringRes(R.string.tip_msg_10_tts), poiInfo.getName());
        entity.setTtsContent(tts);
        notifyUi(entity,false);
        mNaviPackage.playChargeTips(tts, SceneNaviChargeBtnType.SEARCH_NEW_STATION);
    }

    /**
     * 提醒途经点充电站无法抵达,查找新站
     */
    private void chargeStationUnreachableSearchNew(PoiInfoEntity poiInfo) {
        if (poiInfo == null) {
            Logger.i(TAG, "poiInfo == null");
            return;
        }
        final ChargeTipEntity entity = new ChargeTipEntity();
        entity.setTitle(String.format(getStringRes(R.string.tip_msg_5), poiInfo.getName()));
        entity.setAction(getStringRes(R.string.msg_action_search_new));
        entity.setType(SceneNaviChargeBtnType.SEARCH_NEW_STATION);
        String tts = String.format(getStringRes(R.string.tip_msg_5_tts), poiInfo.getName());
        entity.setTtsContent(tts);
        notifyUi(entity,false);
        mNaviPackage.playChargeTips(tts, SceneNaviChargeBtnType.SEARCH_NEW_STATION);
    }

    /**
     * 错过充电站,查找新站
     */
    private void chargeStationPassSearchNew(PoiInfoEntity poiInfo) {
        if (poiInfo == null) {
            Logger.i(TAG, "poiInfo == null");
            return;
        }
        final ChargeTipEntity entity = new ChargeTipEntity();
        entity.setTitle(String.format(getStringRes(R.string.tip_msg_11), poiInfo.getName()));
        entity.setAction(getStringRes(R.string.msg_action_search_new));
        entity.setType(SceneNaviChargeBtnType.SEARCH_NEW_STATION);
        String tts = String.format(getStringRes(R.string.tip_msg_11_tts), poiInfo.getName());
        entity.setTtsContent(tts);
        notifyUi(entity,false);
        mNaviPackage.playChargeTips(tts, SceneNaviChargeBtnType.SEARCH_NEW_STATION);
    }

    /**
     * 错过充电站,已刷新
     */
    private void chargeStationPassUpdate(PoiInfoEntity poiInfo) {
        if (poiInfo == null) {
            Logger.i(TAG, "poiInfo == null");
            return;
        }
        final ChargeTipEntity entity = new ChargeTipEntity();
        entity.setTitle(getStringRes(R.string.tip_msg_3));
        entity.setSubTitle(String.format(getStringRes(R.string.tip_msg_11), poiInfo.getName()));
        entity.setAction(getStringRes(R.string.msg_action_know));
        entity.setType(SceneNaviChargeBtnType.I_KNOW);
        entity.setTtsContent(String.format(getStringRes(R.string.tip_msg_11_tts2), poiInfo.getName()));
        notifyUi(entity, true);
    }

    /**
     * 充电站未营业,已刷新
     */
    private void chargeStationClosedUpdate(PoiInfoEntity poiInfo) {
        if (poiInfo == null) {
            Logger.i(TAG, "poiInfo == null");
            return;
        }
        final ChargeTipEntity entity = new ChargeTipEntity();
        entity.setTitle(getStringRes(R.string.tip_msg_3));
        entity.setSubTitle(String.format(getStringRes(R.string.tip_msg_10), poiInfo.getName()));
        entity.setAction(getStringRes(R.string.msg_action_know));
        entity.setType(SceneNaviChargeBtnType.I_KNOW);
        entity.setTtsContent(String.format(getStringRes(R.string.tip_msg_10_tts2), poiInfo.getName()));
        notifyUi(entity, true);
    }

    /**
     * 充电站无法抵达,已刷新
     */
    private void chargeStationUnreachableUpdate(PoiInfoEntity poiInfo) {
        if (poiInfo == null) {
            Logger.i(TAG, "poiInfo == null");
            return;
        }
        final ChargeTipEntity entity = new ChargeTipEntity();
        entity.setTitle(getStringRes(R.string.tip_msg_3));
        entity.setSubTitle(String.format(getStringRes(R.string.tip_msg_5), poiInfo.getName()));
        entity.setAction(getStringRes(R.string.msg_action_know));
        entity.setType(SceneNaviChargeBtnType.I_KNOW);
        entity.setTtsContent(String.format(getStringRes(R.string.tip_msg_5_tts2), poiInfo.getName()));
        notifyUi(entity, true);
    }

    /**
     * 提醒途经点充电站拥挤，刷新路线
     */
    private void chargeStationCongestionUpdate(RouteAlterChargeStationInfo info) {
        if (info == null) {
            Logger.i(TAG, "info == null");
            return;
        }
        final ChargeTipEntity entity = new ChargeTipEntity();
        entity.setTitle(String.format(getStringRes(R.string.tip_msg_2), info.getMName()) + curViaFree + "/" + curViaTotal);
        entity.setAction(getStringRes(R.string.msg_action_update));
        entity.setType(SceneNaviChargeBtnType.UPDATE_SUPPLY);
        String tts = String.format(getStringRes(R.string.tip_msg_2_tts2), info.getMName());
        entity.setTtsContent(tts);
        entity.setRouteAlterChargeStationInfo(info);
        notifyUi(entity,false);
        mRouteAlterChargeStationInfo = info;
        mNaviPackage.playChargeTips(tts, SceneNaviChargeBtnType.UPDATE_SUPPLY);
    }

    public RouteAlterChargeStationInfo getRouteAlterChargeStationInfo() {
        return mRouteAlterChargeStationInfo;
    }

    /**
     * 剩余里程内沿途充电站数量较少，提醒去充电
     */
    private void chargeStationFewGoCharge() {
        final ChargeTipEntity entity = new ChargeTipEntity();
        entity.setTitle(getStringRes(R.string.tip_msg_6));
        entity.setAction(getStringRes(R.string.msg_action_go_charging));
        entity.setType(SceneNaviChargeBtnType.GO_CHARGING);
        notifyUi(entity, true);
    }

    /**
     * 提醒剩余电量低，是否打开补能规划
     */
    private void notifyPowerLowOpenSupply() {
        final ChargeTipEntity entity = new ChargeTipEntity();
        entity.setTitle(getStringRes(R.string.tip_msg_12));
        entity.setAction(getStringRes(R.string.msg_action_open));
        entity.setType(SceneNaviChargeBtnType.OPEN_SUPPLY);
        entity.setSubTitle(getStringRes(R.string.tip_msg_12_sub_title));
        //entity.setTtsContent(getStringRes(R.string.tip_msg_2_tts));
        notifyUi(entity, true);
    }

    /**
     * 电量预警，不足50公里
     */
    private void lowPowerGoCharge() {
        final ChargeTipEntity entity = new ChargeTipEntity();
        entity.setTitle(getStringRes(R.string.tip_msg_1));
        entity.setAction(getStringRes(R.string.msg_action_go_charging));
        entity.setType(SceneNaviChargeBtnType.GO_CHARGING);
        notifyUi(entity, true);
    }

    /**
     * 预约充电桩将要超时
     */
    public void chargeStationTimeOver(int timeOver) {
        final ChargeTipEntity entity = new ChargeTipEntity();
        entity.setTitle(String.format(getStringRes(R.string.tip_msg_7), timeOver));
        entity.setAction(getStringRes(R.string.msg_action_know));
        entity.setType(SceneNaviChargeBtnType.I_KNOW);
        notifyUi(entity, true);
    }

    /**
     * 添加途径点
     *
     * @param info 替换充电站信息
     */
    public void addViaList(final RouteAlterChargeStationInfo info) {
        if (ConvertUtils.isEmpty(info)) {
            Logger.i(TAG, "info == null");
            return;
        }
        final PoiInfoEntity poiInfoEntity = new PoiInfoEntity();
        final GeoPoint geoPoint = new GeoPoint(info.getMPos().getLon(), info.getMPos().getLat(), info.getMPos().getZ());
        poiInfoEntity.setPid(info.getMPoiId());
        poiInfoEntity.setName(info.getMName());
        poiInfoEntity.setTypeCode("011100");
        poiInfoEntity.setPoint(geoPoint);
        //TODO 回调
        mRoutePackage.addViaPoint(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity);
    }


    /**
     * 判断当前时间是否在指定的非跨天时间段内（开始时间 <= 结束时间）
     *
     * @return 当前时间在时间段内返回true，否则返回false（若开始时间 > 结束时间，直接返回false）
     * @throws IllegalArgumentException 时间格式错误时抛出
     */
    public boolean isCurrentTimeInRange(String time) {
        Logger.i(TAG, "isCurrentTimeInRange time:", time);
        if (ConvertUtils.isEmpty(time)) {
            return true;
        }
        String[] timesDay = time.split(" ");
        if (timesDay.length != 2 || ConvertUtils.isEmpty(timesDay[1])) {
            return true;
        }
        String[] timesH = timesDay[1].split("-");
        if (timesH.length != 2) {
            return true;
        }
        String startTimeStr = timesH[0];
        String endTimeStr = timesH[1];
        if (startTimeStr == null || endTimeStr == null) {
            return true;
        }
        if (!StringUtils.isHHmmFormat(startTimeStr) || !StringUtils.isHHmmFormat(endTimeStr)) {
            return true;
        }
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("HH:mm");
        try {
            // 解析开始和结束时间为LocalTime对象
            LocalTime startTime = LocalTime.parse(startTimeStr, formatter);
            LocalTime endTime = LocalTime.parse(endTimeStr, formatter);
            LocalTime now = LocalTime.now();
            if (startTime == null || endTime == null) {
                return true;
            }
            // 不跨天的前提：开始时间 <= 结束时间
            if (startTime.isAfter(endTime)) {
                Logger.i(TAG, "开始时间 <= 结束时间");
                return true; // 时间段无效（跨天），直接返回false
            }
            DateTimeFormatter nowFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
            // 判断当前时间是否在[startTime, endTime]区间内（包含边界）
            boolean isInRange = !now.isBefore(startTime) && !now.isAfter(endTime);
            Logger.i(TAG, "isInRange:", isInRange, " now:", nowFormatter.format(now));
            return isInRange;
        } catch (DateTimeParseException e) {
            Logger.i(TAG, "时间格式错误，请使用HH:mm格式（例如：08:00）");
        }
        return true;
    }

    /**
     * 查询预约充电站信息 静默搜索
     */
    public void queryReservation() {
        if (mViewModel == null || mViewModel.getActivity() == null) {
            Logger.i(TAG, "mViewModel:", mViewModel == null);
            return;
        }
        AccessTokenParam param = new AccessTokenParam(
                AutoMapConstant.AccountTokenParamType.ACCOUNT_TYPE_PATAC_HMI,
                AutoMapConstant.AccountTokenParamType.AUTH_TOKEN_TYPE_READ_ONLY,
                null,
                mViewModel.getActivity(),
                null,
                null,
                null,
                null);
        ThreadManager.getInstance().runAsync(() -> {
            String idpUserId = AccountPackage.getInstance().getUserId();
            String accessToken = AccountPackage.getInstance().getAccessToken(param);
            String brandId = mSearchPackage.getBrandId(CalibrationPackage.getInstance().brand());
            String preNum = mSearchPackage.getReservationPreNum();
            if (ConvertUtils.isEmpty(idpUserId) || ConvertUtils.isEmpty(accessToken) || ConvertUtils.isEmpty(brandId) || ConvertUtils.isEmpty(preNum)) {
                Logger.i(TAG, "queryReservation idpUserId:", idpUserId, " accessToken:", accessToken, " brandId:", brandId, " preNum:", preNum);
                return;
            }
            mReservationTaskId = mSearchPackage.queryReservationByPreNum(preNum, brandId, idpUserId, accessToken);
        });
    }

    /***
     *
     * @param entity
     */
    private void notifyUi(final ChargeTipEntity entity, boolean isPlayTts) {
        Logger.i(TAG, "notifyUi", "viewModel:" , (mViewModel == null));
        if (mViewModel != null && entity != null) {
            mViewModel.notifyBatteryWarning(entity);
            if (isPlayTts && !TextUtils.isEmpty(entity.getTtsContent())) {
                playTts(entity.getTtsContent());
            }
        }
    }

    /***
     * tts 播放提示
     * @param msg
     */
    public void playTts(final String msg) {
        Logger.i(TAG, "playTts:" , msg);
        if (mSpeechPackage != null && !ConvertUtils.isEmpty(msg)) {
            mSpeechPackage.synthesize(msg);
        }
    }

    /***
     * 获取字符串资源
     * @param resId
     * @return
     */
    private String getStringRes(int resId) {
        return AppCache.getInstance().getMContext().getResources().getString(resId);
    }

    /***
     * 测试代码
     */
    public void mockTest() {
        if (!mTestIsMocking) {
            ChargeTipEntity entity = null;
            entity = new ChargeTipEntity();
            entity.setTitle(String.format(getStringRes(com.sgm.navi.scene.R.string.tip_msg_5), "测试充电桩"));
            entity.setAction(getStringRes(com.sgm.navi.scene.R.string.msg_action_search_new));
            entity.setType(SceneNaviChargeBtnType.SEARCH_NEW_STATION);
            entity.setTtsContent(String.format(getStringRes(com.sgm.navi.scene.R.string.tip_msg_5_tts), "测试充电桩"));
            notifyUi(entity, true);
            mTestIsMocking = true;
        } else {
            Logger.i(TAG, "mock 完成！");
        }
    }

    /**
     * 提醒消息类型
     */
    public enum TipType {
        Invalid,      //未初始化
        Congestion,   //充电站拥挤
        Closed,       //充电站未营业
        Pass,         //错过充电站
        Unreachable   //充电站无法到达
    }
}

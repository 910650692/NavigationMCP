package com.fy.navi.hmi.route;

import android.os.Bundle;

import androidx.fragment.app.Fragment;

import com.alibaba.android.arouter.launcher.ARouter;
import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.TimeUtils;
import com.android.utils.ToastUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.bean.BuryProperty;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.burypoint.controller.BuryPointController;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.limit.LimitDriveFragment;
import com.fy.navi.hmi.poi.PoiDetailsFragment;
import com.fy.navi.hmi.search.parking.TerminalParkingFragment;
import com.fy.navi.hmi.traffic.TrafficEventFragment;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.layer.GemBaseLayer;
import com.fy.navi.service.define.layer.GemLayerClickBusinessType;
import com.fy.navi.service.define.layer.GemLayerItem;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.layer.refix.LayerType;
import com.fy.navi.service.define.map.MapStateStyle;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.map.MapTypeManager;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.route.EvRangeOnRouteInfo;
import com.fy.navi.service.define.route.RequestRouteResult;
import com.fy.navi.service.define.route.RouteAlongCityInfo;
import com.fy.navi.service.define.route.RouteAlongCityParam;
import com.fy.navi.service.define.route.RouteChargeStationDetailInfo;
import com.fy.navi.service.define.route.RouteChargeStationInfo;
import com.fy.navi.service.define.route.RouteChargeStationParam;
import com.fy.navi.service.define.route.RouteLineInfo;
import com.fy.navi.service.define.route.RouteLineSegmentInfo;
import com.fy.navi.service.define.route.RouteMsgPushInfo;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.route.RoutePoiType;
import com.fy.navi.service.define.route.RoutePriorityType;
import com.fy.navi.service.define.route.RouteRequestParam;
import com.fy.navi.service.define.route.RouteRestAreaDetailsInfo;
import com.fy.navi.service.define.route.RouteRestAreaInfo;
import com.fy.navi.service.define.route.RouteRestAreaParam;
import com.fy.navi.service.define.route.RouteRestTollGateParam;
import com.fy.navi.service.define.route.RouteRestirctionID;
import com.fy.navi.service.define.route.RouteRestrictionInfo;
import com.fy.navi.service.define.route.RouteRestrictionParam;
import com.fy.navi.service.define.route.RouteSpeechRequestParam;
import com.fy.navi.service.define.route.RouteTrafficIncidentParam;
import com.fy.navi.service.define.route.RouteWayID;
import com.fy.navi.service.define.route.RouteWeatherInfo;
import com.fy.navi.service.define.route.RouteWeatherParam;
import com.fy.navi.service.define.search.ETAInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.define.utils.BevPowerCarUtils;
import com.fy.navi.service.define.utils.NumberUtils;
import com.fy.navi.service.greendao.setting.SettingManager;
import com.fy.navi.service.logicpaket.layer.ILayerPackageCallBack;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.mapdata.MapDataPackage;
import com.fy.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.fy.navi.service.logicpaket.route.IRouteResultObserver;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.search.SearchResultCallback;
import com.fy.navi.ui.base.BaseFragment;
import com.fy.navi.ui.base.BaseModel;
import com.fy.navi.ui.base.StackManager;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;

public class RouteModel extends BaseModel<RouteViewModel> implements IRouteResultObserver, ILayerPackageCallBack
        , ImmersiveStatusScene.IImmersiveStatusCallBack, SearchResultCallback {
    private static final String TAG = RouteModel.class.getSimpleName();
    private final SearchPackage mSearchPackage;
    private RoutePackage mRoutePackage;
    private LayerPackage mLayerPackage;
    private List<RouteLineInfo> mRouteLineInfos;
    private List<RouteWeatherInfo> mRouteWeatherInfos;
    private RouteChargeStationParam mRouteChargeStationParam;
    private List<RouteRestrictionInfo> mRouteRestrictionInfo;
    private List<RouteAlongCityInfo> mRouteAlongCityInfos;
    private List<RouteParam> mRouteParams;
    private Map<Integer, PoiInfoEntity> mTaskMap = new HashMap<>();
    private long mRestirctionTaskId;
    private RouteRestrictionParam mRouteRestrictionParams;
    private RequestRouteResult mRequestRouteResults;
    private boolean mIsFirstRequest = true;

    private List<RouteParam> mGasChargeAlongList = new ArrayList<>();

    private int mLocalTaskId = -1;
    private int mListSearchType;
    private List<RouteRestAreaInfo> mRouteRestAreaInfos;

    public RouteModel() {
        mLayerPackage = LayerPackage.getInstance();
        mRoutePackage = RoutePackage.getInstance();
        mRoutePackage.registerRouteObserver(TAG, this);
        mSearchPackage = SearchPackage.getInstance();
        MapPackage.getInstance().setMapStateStyle(MapType.MAIN_SCREEN_MAIN_MAP, MapStateStyle.MAP_ROUTING);
    }

    @Override
    public void onCreate() {
        super.onCreate();
    }

    @Override
    public void onStart() {
        super.onStart();
        Logger.d(TAG, "registerCallBack");
        ImmersiveStatusScene.getInstance().registerCallback(TAG, this);
        mLayerPackage.registerCallBack(MapType.MAIN_SCREEN_MAIN_MAP, this);
        mSearchPackage.registerCallBack(TAG, this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        Logger.d(TAG, "unRegisterCallBack");
        mRoutePackage.clearRestrictionView(MapType.MAIN_SCREEN_MAIN_MAP);
        mRoutePackage.unRegisterRouteObserver(TAG);
        mLayerPackage.unRegisterCallBack(MapType.MAIN_SCREEN_MAIN_MAP, this);
        mSearchPackage.unRegisterCallBack(TAG);
    }

    /**
     * 请求算路
     * @param param 请求参数
     * */
    public void requestRouteMode(final RouteRequestParam param) {
        param.setMMapTypeId(MapType.MAIN_SCREEN_MAIN_MAP);
        mRoutePackage.requestRoute(param);
    }

    /**
     * send2car 请求算路
     * @param routeMsgPushInfo 请求参数
     * */
    public void requestRouteRestoration(final RouteMsgPushInfo routeMsgPushInfo) {
        mRoutePackage.requestRouteRestoration(routeMsgPushInfo, MapType.MAIN_SCREEN_MAIN_MAP);
    }

    /**
     * 语音 请求算路
     * @param param 请求参数
     * */
    public void requestRouteFromSpeech(final RouteSpeechRequestParam param) {
        mRoutePackage.requestRouteFromSpeech(param);
    }

    /**
     * 修改终点算路请求
     *
     * @param poiInfoEntity 终点数据
     */
    public void requestChangeEnd(final PoiInfoEntity poiInfoEntity) {
        mRoutePackage.requestChangeEnd(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity);
    }

    /**
     * 清除路线信息
     * */
    public void clearRouteLine() {
        mRoutePackage.clearRouteLine(MapType.MAIN_SCREEN_MAIN_MAP);
    }

    /**
     * 获取路线导航段数据
     * @param index 列表索引
     * @return 导航段数据
     * */
    public List<RouteLineSegmentInfo> getDetailsResult(final int index) {
        if (ConvertUtils.isEmpty(mRouteLineInfos) || mRouteLineInfos.isEmpty()) {
            return new ArrayList<>();
        }
        return mRouteLineInfos.get(index).getMRouteLineSegmentInfos();
    }

    /**
     * 请求天气数据
     * */
    public void getWeatherList() {
        if (!ConvertUtils.isEmpty(mViewModel)) {
            mViewModel.showSearchProgressUI();
        }
        if (getCurrentIndex() == -1) {
            Logger.e(TAG, "天气请求失败");
            return;
        }
        mRoutePackage.requestRouteWeather(MapType.MAIN_SCREEN_MAIN_MAP, getCurrentIndex());
    }

    /**
     * 隐藏天气数据
     * */
    public void hideWeatherList() {
        mRoutePackage.clearWeatherView(MapType.MAIN_SCREEN_MAIN_MAP);
        //hide weather list 不需要
        if (!ConvertUtils.isEmpty(mViewModel)) {
            mViewModel.hideWeatherDeatilsUI();
        }
    }

    /**
     * 获取服务区列表
     * */
    public void getSearchListAndShow() {
        clearSearchLabel();
        if (!ConvertUtils.isEmpty(mViewModel)) {
            mViewModel.showSearchProgressUI();
        }
        if (ConvertUtils.isEmpty(mRouteRestAreaInfos)) {
            return;
        }
        final RouteRestAreaInfo routeRestAreaInfo = mRouteRestAreaInfos.get(getCurrentIndex());
        if (ConvertUtils.isEmpty(routeRestAreaInfo)) {
            return;
        }
        final List<RouteRestAreaDetailsInfo> routeRestAreaDetailsInfos = routeRestAreaInfo.getMRouteRestAreaDetailsInfos();
        if (!ConvertUtils.isEmpty(routeRestAreaDetailsInfos)
                && !ConvertUtils.isEmpty(mRouteLineInfos)
                && !ConvertUtils.isEmpty(mRouteLineInfos.get(getCurrentIndex()))) {
            final long dis = mRouteLineInfos.get(getCurrentIndex()).getMDistance();
            for (RouteRestAreaDetailsInfo info : routeRestAreaDetailsInfos) {
                final long remain = dis - info.getMRemainDist();
                info.setMRemainDist(remain);
            }
        }
        if (!ConvertUtils.isEmpty(mViewModel)) {
            mViewModel.showRouteSearchListUI(routeRestAreaDetailsInfos);
        }
        showRestArea();
        if (!ConvertUtils.isEmpty(mViewModel)) {
            mViewModel.hideSearchProgressUI();
        }
        if (ConvertUtils.isEmpty(routeRestAreaDetailsInfos)) {
            ToastUtils.Companion.getInstance().showCustomToastView(
                    ResourceUtils.Companion.getInstance().getString(R.string.route_error_no_service_data));
        }
    }

    /**
     * 请求充电站列表
     * @param keyWord  关键字
     * @param searchType 搜索方式
     * */
    public void getSearchListChargeAndShow(final String keyWord, final int searchType) {
        mListSearchType = searchType;
        clearSearchLabel();
        if (!ConvertUtils.isEmpty(mViewModel)) {
            mViewModel.showSearchProgressUI();
        }
        if (searchType == 0) {
            mLocalTaskId = mSearchPackage.enRouteKeywordSearch(keyWord);
        } else if (searchType == 1) {
            final RouteParam endPoint = mRoutePackage.getEndPoint(MapType.MAIN_SCREEN_MAIN_MAP);
            mLocalTaskId = mSearchPackage.aroundSearch(1, keyWord, new GeoPoint(endPoint.getRealPos().getLon(), endPoint.getRealPos().getLat()));
        } else {
            mLocalTaskId = mSearchPackage.aroundSearch(1, keyWord);
        }
    }

    /**
     * 请求详情数据
     * @param poiInfoEntity  请求参数
     * */
    public void getSearchDetailsMode(final PoiInfoEntity poiInfoEntity) {
        mTaskMap.clear();
        if (!ConvertUtils.isEmpty(mViewModel)) {
            mViewModel.showSearchProgressUI();
        }
        final int pointType = mSearchPackage.getPointTypeCode(poiInfoEntity.getPointTypeCode());
        if (poiInfoEntity.getPoiTag().equals(ResourceUtils.Companion.getInstance().getString(R.string.route_search_keyword_service))
                || pointType == AutoMapConstant.PointTypeCode.SERVICE_AREA) {
            final List<String> poiIds = new ArrayList<>();
            poiIds.add(poiInfoEntity.getPid());
            mLocalTaskId = mSearchPackage.doLineDeepInfoSearch(ResourceUtils.Companion.getInstance()
                    .getString(R.string.route_search_keyword_service), poiIds);
        } else if (poiInfoEntity.getPoiTag().equals(ResourceUtils.Companion.getInstance().getString(R.string.route_search_keyword_charge))
                || pointType == AutoMapConstant.PointTypeCode.CHARGING_STATION) {
            mLocalTaskId = mSearchPackage.poiIdSearch(poiInfoEntity.getPid());
        }
        mTaskMap.put(mLocalTaskId, poiInfoEntity);
    }

    /**
     * 请求服务区详情地址
     * @param poiInfoEntity  请求参数
     * */
    public void getAddress(final PoiInfoEntity poiInfoEntity) {
        mTaskMap.clear();
        mLocalTaskId = mSearchPackage.deppInfoSearch(poiInfoEntity.getPid()
                , new GeoPoint(poiInfoEntity.getPoint().getLon(), poiInfoEntity.getPoint().getLat()));
        mTaskMap.put(mLocalTaskId, poiInfoEntity);
    }

    /**
     * 充电站本地移除
     * @param poiInfoEntity  请求参数
     * */
    public void gasChargeRemoveMode(final PoiInfoEntity poiInfoEntity) {
        if (mListSearchType == 0) {
            if (mRoutePackage.isStartOrEndRouteParam(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity)) {
                ToastUtils.Companion.getInstance().showCustomToastView(
                        ResourceUtils.Companion.getInstance().getString(R.string.route_error_add_start_end));
                return;
            }
            RouteParam routeParams = new RouteParam();
            for (RouteParam routeParam : mGasChargeAlongList) {
                if (mRoutePackage.isTheSamePoi(routeParam, poiInfoEntity)) {
                    routeParams = routeParam;
                    break;
                }
            }
            //去除进度条扎点
            if (!ConvertUtils.isEmpty(mViewModel)) {
                mViewModel.removeRouteChargePoiUi(poiInfoEntity);
            }
            mGasChargeAlongList.remove(routeParams);
            if (!ConvertUtils.isEmpty(mViewModel)) {
                mViewModel.updateChareList(mGasChargeAlongList, mListSearchType);
            }
        } else {
            mRoutePackage.removeVia(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity, false);
        }
    }

    /**
     * 充电站本地添加
     * @param poiInfoEntity  请求参数
     * */
    public void gasChargeAddMode(final PoiInfoEntity poiInfoEntity) {
        if (mListSearchType == 0) {
            if (mGasChargeAlongList.size() >= 5) {
                ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.route_error_via_max));
                return;
            }
            //绘制充电站在进度条上的扎点
            if (!ConvertUtils.isEmpty(mViewModel)) {
                mViewModel.addRouteChargePoiUi(poiInfoEntity);
            }
            mGasChargeAlongList.add(mRoutePackage.getRouteParamFromPoiInfoEntity(poiInfoEntity, RoutePoiType.ROUTE_POI_TYPE_WAY));
            if (!ConvertUtils.isEmpty(mViewModel)) {
                mViewModel.updateChareList(mGasChargeAlongList, mListSearchType);
            }
        } else {
            mRoutePackage.addViaPoint(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity);
        }
    }

    /**
     * 同时添加多个途径带点
     * */
    public void startAllRequest() {
        if (mGasChargeAlongList.size() > 5) {
            ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.route_error_via_max));
            return;
        }
        mRoutePackage.requestManyVia(MapType.MAIN_SCREEN_MAIN_MAP, mGasChargeAlongList);
    }

    /**
     * 获取当前点到此点的时间和距离
     * @param geoPoint 目标点
     * @return 时间和距离
     * */
    public CompletableFuture<ETAInfo> getTravelTimeFuture(final GeoPoint geoPoint) {
        return mSearchPackage.getTravelTimeFutureIncludeChargeLeft(geoPoint);
    }

    /**
     * 打开限行页面
     * */
    public void showRestrictionFragment() {
        if (ConvertUtils.isEmpty(mRouteRestrictionParams)) {
            return;
        }
        final Bundle bundle = new Bundle();
        bundle.putSerializable(AutoMapConstant.CommonBundleKey.BUNDLE_KEY_LIMIT_ROUND, mRouteRestrictionParams);
        addFragment(new LimitDriveFragment(), bundle);
    }

    /**
     * 清除限行图层
     * */
    public void clearRestrictionView() {
        mRoutePackage.clearRestrictionView(MapType.MAIN_SCREEN_MAIN_MAP);
    }

    /**
     * 清除搜索扎点
     * */
    public void clearSearchLabel() {
        mSearchPackage.clearLabelMark();
    }

    /**
     * 清除天气图层
     * */
    public void clearWeatherView() {
        mRoutePackage.clearWeatherView(MapType.MAIN_SCREEN_MAIN_MAP);
    }

    /**
     * 展示服务区扎点
     * */
    public void showRestArea() {
        mRoutePackage.showRestArea(MapType.MAIN_SCREEN_MAIN_MAP, getCurrentIndex());
    }

    /**
     * 清除服务区图层
     * */
    public void clearRestArea() {
        mRoutePackage.clearRestArea(MapType.MAIN_SCREEN_MAIN_MAP);
    }

    /***
     * 排序途经点列表
     * @param currentPosition 当前索引
     * @param movePosition 移动的索引
     */
    public void changeParamListMode(final int currentPosition, final int movePosition) {
        if (mRouteParams.size() < 2) {
            return;
        }
        if (currentPosition < 0) {
            return;
        }
        if (movePosition < 0) {
            return;
        }
        if (ConvertUtils.isEmpty(mRouteParams)) {
            return;
        }
        if (mRouteParams.size() <= currentPosition || mRouteParams.size() <= movePosition) {
            return;
        }
        Collections.swap(mRouteParams, currentPosition, movePosition);
        mRoutePackage.updateViaParamList(MapType.MAIN_SCREEN_MAIN_MAP, mRouteParams);
    }

    /***
     * 删除途经点
     * @param index 索引
     */
    public void deleteViaParamMode(final int index) {
        if (ConvertUtils.isEmpty(mRouteParams)) {
            return;
        }
        if (mRouteParams.size() <= index) {
            return;
        }
        mRouteParams.remove(index);
        mRoutePackage.updateViaParamList(MapType.MAIN_SCREEN_MAIN_MAP, mRouteParams);
        final RouteRequestParam param = new RouteRequestParam();
        param.setMRouteWay(RouteWayID.ROUTE_WAY_DELETE_VIA);
        param.setMRoutePriorityType(RoutePriorityType.ROUTE_TYPE_CHANGE_JNY_PNT);
        requestRouteMode(param);
    }

    /***
     * 添加途经点
     * @param poiInfoEntity poi数据
     */
    public void addViaList(final PoiInfoEntity poiInfoEntity) {
        mRoutePackage.addViaPoint(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity);
    }

    /***
     * 删除途经点
     * @param poiInfoEntity poi数据
     */
    public void deleteViaParamMode(final PoiInfoEntity poiInfoEntity) {
        mRoutePackage.removeVia(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity, true);
    }

    public String getPlateNumber() {
        return SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_GUIDE_VEHICLE_NUMBER);
    }

    public String getAvoidLimit() {
        return SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_GUIDE_AVOID_LIMIT);
    }

    public String getPreferences() {
        return SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_GUIDE_ROUTE_PREFERENCE);
    }

    public String getEnergy() {
        return SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_GUIDE_CHARGING_PLAN);
    }

    @Override
    public void onRouteSuccess(final String successMsg) {
        mRouteLineInfos = null;
        mRouteWeatherInfos = null;
        mRouteRestrictionInfo = null;
        mRouteAlongCityInfos = null;
        mIsFirstRequest = false;
        if (!ConvertUtils.isEmpty(successMsg)) {
            ThreadManager.getInstance().postUi(() -> {
                ToastUtils.Companion.getInstance().showCustomToastView(successMsg);
            });
        }
    }

    /***
     * 路线被选中
     * @param index 路线索引
     */
    public void onRouteSelect(final int index) {
        if (ConvertUtils.isEmpty(mRouteRestrictionInfo)) {
            if (!ConvertUtils.isEmpty(mViewModel)) {
                mViewModel.updateRestrictionTextUI(-1);
            }
            return;
        }
        if (!ConvertUtils.isEmpty(mViewModel)) {
            mViewModel.updateRestrictionTextUI((int) mRouteRestrictionInfo.get(index).getMTitleType());
        }
    }

    /***
     * 获取能量耗尽点信息
     * @param index 路线索引
     * @return 能量耗尽点信息
     */
    public EvRangeOnRouteInfo getRangeOnRouteInfo(final int index) {
        return mRoutePackage.getEvRangeOnRouteInfos().get(index);
    }

    /***
     * 是否是路上的点
     * @param poiInfoEntity poi数据
     * @return 是否在路径上
     */
    public boolean isBelongRouteParam(final PoiInfoEntity poiInfoEntity) {
        return mRoutePackage.isBelongRouteParam(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity);
    }

    /**
     * 获取选中的路线信息
     * @return 路线信息
     * */
    public RouteLineInfo getSelectLineInfo() {
        if (!ConvertUtils.isEmpty(mRouteLineInfos)) {
            return mRouteLineInfos.get(getCurrentIndex());
        }
        return null;
    }

    /**
     * 获取选中的路线信息
     * @return 路线信息
     * */
    public int getCurrentIndex() {
        return mRoutePackage.getSelectRouteIndex().get(MapType.MAIN_SCREEN_MAIN_MAP);
    }

    /**
     * 取消算路
     * */
    public void cancelRoute() {
        mRoutePackage.abortRequest(MapType.MAIN_SCREEN_MAIN_MAP);
    }

    @Override
    public void onRouteResult(final RequestRouteResult requestRouteResult) {
        if (ConvertUtils.isEmpty(requestRouteResult)) {
            return;
        }
        mRequestRouteResults = requestRouteResult;
        mRouteLineInfos = requestRouteResult.getMRouteLineInfos();
        if (mRouteLineInfos.size() <= NumberUtils.NUM_0) {
            return;
        }
        if (!ConvertUtils.isEmpty(mViewModel)) {
            mViewModel.setRouteResultListUI(mRouteLineInfos);
        }
        showUIOnlineOffline(requestRouteResult.isMIsOnlineRoute());
    }

    /***
     * 展示在线/离线UI变化
     * @param onlineRoute 是否在线
     */
    private void showUIOnlineOffline(final boolean onlineRoute) {
        //离线
        if (Boolean.FALSE.equals(onlineRoute)) {
            //限行
            if (!ConvertUtils.isEmpty(mViewModel)) {
                mViewModel.updateRestrictionTextUI(RouteRestirctionID.REATIRCTION_LIMITTIPSTYPENETWORK);
            }
        }
    }

    @Override
    public void onRouteDrawLine(final RouteLineLayerParam routeLineLayerParam) {
        mRoutePackage.showRouteLine(routeLineLayerParam.getMMapTypeId());
        mRoutePackage.selectRoute(routeLineLayerParam.getMMapTypeId(), NumberUtils.NUM_0);
        ImmersiveStatusScene.getInstance().setImmersiveStatus(MapType.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.IMERSIVE);
        if (!ConvertUtils.isEmpty(mViewModel)) {
            mViewModel.hideProgressUI(true);
        }
    }

    @Override
    public void onRouteRestAreaInfo(final RouteRestAreaParam routeRestAreaParam) {
        if (ConvertUtils.isEmpty(routeRestAreaParam)) {
            return;
        }
        mRouteRestAreaInfos = routeRestAreaParam.getMRouteRestAreaInfos();
        final boolean hasService = !ConvertUtils.isEmpty(mRouteRestAreaInfos)
                && mRouteRestAreaInfos.size() > getCurrentIndex()
                && getCurrentIndex() != -1
                && !ConvertUtils.isEmpty(mRouteRestAreaInfos.get(getCurrentIndex()).getMRouteRestAreaDetailsInfos());
        final boolean atLeastDistance = !ConvertUtils.isEmpty(mRouteLineInfos)
                && mRouteLineInfos.size() > getCurrentIndex()
                && getCurrentIndex() != -1
                && !ConvertUtils.isEmpty(mRouteLineInfos.get(getCurrentIndex()))
                && mRouteLineInfos.get(getCurrentIndex()).getMDistance() >= 50 * 1000;
        BevPowerCarUtils.getInstance().isLongRoute = hasService || atLeastDistance;
        if (!ConvertUtils.isEmpty(mViewModel)) {
            mViewModel.showHideTab(BevPowerCarUtils.getInstance().isLongRoute);
        }
    }

    @Override
    public void onRouteWeatherInfo(final RouteWeatherParam routeWeatherParam) {
        if (!ConvertUtils.isEmpty(mViewModel)) {
            mViewModel.hideSearchProgressUI();
        }
        if (ConvertUtils.isEmpty(routeWeatherParam)) {
            ThreadManager.getInstance().postUi(() -> ToastUtils.Companion.getInstance().showCustomToastView(
                    ResourceUtils.Companion.getInstance().getString(R.string.route_error_wearher)));
            return;
        }
        mRouteWeatherInfos = routeWeatherParam.getMRouteWeatherInfos();
        if (!ConvertUtils.isEmpty(mRouteWeatherInfos) && mRouteWeatherInfos.size() > 0) {
            for (RouteWeatherInfo info : mRouteWeatherInfos) {
                info.setMCityName(MapDataPackage.getInstance().getCityInfo(info.getMCityID()).getName());
            }
        }
    }

    @Override
    public void onRouteRestrictionInfo(final RouteRestrictionParam routeRestrictionParam) {
        mRestirctionTaskId = mRoutePackage.requestRestirction(routeRestrictionParam.getMMapTypeId());
        mRouteRestrictionInfo = routeRestrictionParam.getMRouteRestrictionInfo();
        if (ConvertUtils.isEmpty(mRouteRestrictionInfo)
                || getCurrentIndex() == -1
                || mRouteLineInfos.size() < getCurrentIndex()
                || ConvertUtils.isEmpty(mRouteRestrictionInfo.get(getCurrentIndex()))) {
            if (!ConvertUtils.isEmpty(mViewModel)) {
                mViewModel.updateRestrictionTextUI(RouteRestirctionID.REATIRCTION_LIMITTIPSTYPEINVALID);
            }
            return;
        }
        if (!ConvertUtils.isEmpty(mViewModel)) {
            mViewModel.updateRestrictionTextUI((int) mRouteRestrictionInfo.get(getCurrentIndex()).getMTitleType());
        }
    }

    @Override
    public void onRouteDrawReStrictedAreaInfo(final RouteRestrictionParam routeRestrictionParam) {
        if (ConvertUtils.isEmpty(routeRestrictionParam.getMRestrictedArea())) {
            return;
        }
        if (routeRestrictionParam.getMRestrictedArea().getMRequestId() == mRestirctionTaskId) {
            mRoutePackage.clearRestrictionView(MapType.MAIN_SCREEN_MAIN_MAP);
            mRoutePackage.showRestrictionView(MapType.MAIN_SCREEN_MAIN_MAP, routeRestrictionParam.getMReStrictedAreaResponseParam());
            mRouteRestrictionParams = routeRestrictionParam;
        }
    }

    @Override
    public void onRouteRestTollGateInfo(final RouteRestTollGateParam routeRestTollGateParam) {
    }

    @Override
    public void onRouteCityInfo(final RouteAlongCityParam routeAlongCityParam) {
        //长途判断
        if (ConvertUtils.isEmpty(routeAlongCityParam)) {
            return;
        }
        mRouteAlongCityInfos = routeAlongCityParam.getMRouteAlongCityInfos();
//        if (isFirstRequest) showParkingFragment();
//        isFirstRequest = false;
    }

    /***
     * 打开停车场推荐
     */
    private void showParkingFragment() {
        if (!ConvertUtils.isEmpty(mViewModel)) {
            mViewModel.cancelTimer();
        }
        final Bundle bundle = new Bundle();
        final GeoPoint point = new GeoPoint(mRoutePackage.getEndPoint(MapType.MAIN_SCREEN_MAIN_MAP).getRealPos().getLon()
                , mRoutePackage.getEndPoint(MapType.MAIN_SCREEN_MAIN_MAP).getRealPos().getLat());
        bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, point);
        addFragment(new TerminalParkingFragment(), bundle);
    }

    @Override
    public void onRouteTrafficIncidentInfo(final RouteTrafficIncidentParam routeTrafficIncidentParam) {
    }

    @Override
    public void onRouteAllRoutePoiInfo(final RequestRouteResult requestRouteResult) {
        mRouteParams = requestRouteResult.getMRouteParams();
        if (mRouteParams.size() >= NumberUtils.NUM_2) {
            mRouteParams.remove(mRouteParams.size() - NumberUtils.NUM_1);
            mRouteParams.remove(NumberUtils.NUM_0);
        }
        if (!ConvertUtils.isEmpty(mViewModel)) {
            ThreadManager.getInstance().postUi(() -> mViewModel.setViaListUI(mRouteParams));
        }
    }

    @Override
    public void onRouteFail(final MapType mapTypeId, final String errorMsg) {
        if (!ConvertUtils.isEmpty(mViewModel)) {
            mViewModel.hideProgressUI(false);
        }
        if (!ConvertUtils.isEmpty(errorMsg)) {
            ThreadManager.getInstance().postUi(() -> {
                ToastUtils.Companion.getInstance().showCustomToastView(errorMsg);
            });
            if (!NaviStatusPackage.getInstance().isGuidanceActive()) {
                NaviStatusPackage.getInstance().setNaviStatus(NaviStatus.NaviStatusType.SELECT_ROUTE);
            }
        }
        if (mIsFirstRequest) {
            Logger.i(TAG, "第一次进入算路失败");
            closeFragment(true);
            NaviStatusPackage.getInstance().setNaviStatus(NaviStatus.NaviStatusType.NO_STATUS);
        }
    }

    @Override
    public void onRouteRequest() {
        mSearchPackage.clearLabelMark();
        clearWeatherView();
        clearRestArea();
        clearRestrictionView();
        if (!ConvertUtils.isEmpty(mViewModel)) {
            mViewModel.showProgressUI();
            mViewModel.showNomalRouteUI();
        }
    }

    @Override
    public void onRouteSlected(final MapType mapTypeId, final int routeIndex) {
        if (mapTypeId == MapType.MAIN_SCREEN_MAIN_MAP) {
            if (!ConvertUtils.isEmpty(mViewModel)) {
                mViewModel.updateSelectRouteUI(routeIndex);
            }
            mSearchPackage.clearLabelMark();
            clearWeatherView();
            clearRestArea();
            if (!ConvertUtils.isEmpty(mViewModel)) {
                if (routeIndex >= 0 && routeIndex < mRouteLineInfos.size()
                        && NaviStatusPackage.getInstance().getCurrentNaviStatus().equals(NaviStatus.NaviStatusType.SELECT_ROUTE)) {
                    if (!mRouteLineInfos.get(routeIndex).isMCanBeArrive()) {
                        mViewModel.showTripDialog(ResourceUtils.Companion.getInstance().getString(R.string.route_trip_title)
                                , ResourceUtils.Companion.getInstance().getString(R.string.route_trip_elec_not_arrive));
                    } else if (mRouteLineInfos.get(routeIndex).getMRemainPercent() < 20) {
                        mViewModel.showTripDialog(ResourceUtils.Companion.getInstance().getString(R.string.route_trip_title)
                                , ResourceUtils.Companion.getInstance().getString(R.string.route_trip_elec_small));
                    }
                    final boolean isLongRoute = BevPowerCarUtils.getInstance().isLongRoute;
                    mViewModel.showHideTab(isLongRoute);
                    if (mRouteLineInfos.get(routeIndex).getMRemainPercent() < 20) {
                         mViewModel.showHideElicCheckBox(true);
                    } else {
                         mViewModel.showHideElicCheckBox(false);
                    }
                }
            }
        }
    }

    @Override
    public void onRouteChargeStationInfo(final RouteChargeStationParam routeChargeStationParam) {
        mRouteChargeStationParam = routeChargeStationParam;
    }

    @Override
    public void onRouteItemClick(MapType mapTypeId, final GemLayerItem item) {
        Logger.e(TAG, "onRouteItemClick-RouteModel");
        if (ConvertUtils.isEmpty(item)) {
            return;
        }
        if (!NaviStatus.NaviStatusType.SELECT_ROUTE.equals(NaviStatusPackage.getInstance().getCurrentNaviStatus())) {
            Logger.i(TAG, "is not on route page");
            return;
        }
        switch (item.getClickBusinessType()) {
            case BizRouteTypePath:
                if (!ConvertUtils.isEmpty(mRouteLineInfos)) {
                    int index = -1;
                    for (int t = 0; t < mRouteLineInfos.size(); t++) {
                        if (item.getIndex() == mRouteLineInfos.get(t).getMPathID()) {
                            index = t;
                        }
                    }
                    if (index != -1 && index != getCurrentIndex()) {
                        mRoutePackage.selectRoute(MapType.MAIN_SCREEN_MAIN_MAP, index);
                    }
                }
                break;
            case BizRouteTypeStartPoint:
            case BizRouteTypeViaPoint:
            case BizRouteTypeEndPoint:
                PoiInfoEntity poiInfo = new PoiInfoEntity();
                poiInfo.setPoint(new GeoPoint(item.getLog(), item.getLat()));
                Bundle poiBundle = new Bundle();
                poiBundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_DETAIL, poiInfo);
                poiBundle.putInt(AutoMapConstant.PoiBundleKey.BUNDLE_KEY_START_POI_TYPE, AutoMapConstant.PoiType.POI_MAP_CLICK);
                PoiDetailsFragment poiFragment = new PoiDetailsFragment();
                addPoiDetailsFragment(poiFragment, poiBundle);
                break;
            case BizRouteTypeWeather:
                if (!ConvertUtils.isEmpty(mRouteWeatherInfos) && mRouteWeatherInfos.size() > item.getIndex()) {
                    if (!ConvertUtils.isEmpty(mViewModel)) {
                        mViewModel.showWeatherDeatilsUI(mRouteWeatherInfos.get((int) (item.getIndex())));
                    }
                }
                break;
            case BizRouteTypeRestArea:
                if (!ConvertUtils.isEmpty(mRouteRestAreaInfos) && mRouteRestAreaInfos.size() > getCurrentIndex()
                        && getCurrentIndex() != -1
                        && !ConvertUtils.isEmpty(mRouteRestAreaInfos.get(getCurrentIndex()))
                        && !ConvertUtils.isEmpty(mRouteRestAreaInfos.get(getCurrentIndex()).getMRouteRestAreaDetailsInfos())) {
                    final PoiInfoEntity poiEntryFromService = getPoiEntryFromService(
                            mRouteRestAreaInfos.get(getCurrentIndex()).getMRouteRestAreaDetailsInfos().get((int) (item.getIndex())));
                    getSearchDetailsMode(poiEntryFromService);
                }
                break;
            case BizRouteTypeViaChargeStationPoint:
                if (mRouteChargeStationParam == null) {
                    return;
                }
                final ArrayList<RouteChargeStationInfo> routeChargeStationInfos = mRouteChargeStationParam.getMRouteChargeStationInfos();
                if (routeChargeStationInfos == null) {
                    return;
                }
                final ArrayList<RouteChargeStationDetailInfo> routeChargeStationDetailInfo = routeChargeStationInfos.get(getCurrentIndex())
                        .getMRouteChargeStationDetailInfo();
                if (routeChargeStationDetailInfo == null || routeChargeStationDetailInfo.size() <= item.getIndex()) {
                    return;
                }
                final Fragment chargeFragment = (Fragment) ARouter.getInstance().build(RoutePath.Route.ALTER_CHARGE_FRAGMENT).navigation();
                final Bundle chargeBundle = new Bundle();
                chargeBundle.putSerializable(AutoMapConstant.RouteBundleKey.BUNDLE_KEY_ALTER_CHARGE_STATION,
                        routeChargeStationDetailInfo.get((int) item.getIndex()));
                closeAllFragmentsUntilTargetFragment("AlterChargeFragment");
                addFragment((BaseFragment) chargeFragment, chargeBundle);
                break;
            default:
                break;
    }
}

//    @Override
//    public void onNotifyClick(final MapType mapTypeId, final GemBaseLayer layer, final GemLayerItem item) {
//        if (item.getClickBusinessType() == GemLayerClickBusinessType.BizRouteTypeWeather) {
//            if (!ConvertUtils.isEmpty(mRouteWeatherInfos) && mRouteWeatherInfos.size() > item.getIndex()) {
//                mViewModel.showWeatherDeatilsUI(mRouteWeatherInfos.get((int) (item.getIndex())));
//            }
//        } else if (item.getClickBusinessType() == GemLayerClickBusinessType.BizRouteTypeRestArea) {
//            if (!ConvertUtils.isEmpty(mRouteRestAreaInfos) && mRouteRestAreaInfos.size() > getCurrentIndex()
//                    && !ConvertUtils.isEmpty(mRouteRestAreaInfos.get(getCurrentIndex()))
//                    && !ConvertUtils.isEmpty(mRouteRestAreaInfos.get(getCurrentIndex()).getMRouteRestAreaDetailsInfos())) {
//                final PoiInfoEntity poiEntryFromService = getPoiEntryFromService(
//                        mRouteRestAreaInfos.get(getCurrentIndex()).getMRouteRestAreaDetailsInfos().get((int) (item.getIndex())));
//                getSearchDetailsMode(poiEntryFromService);
//            }
//        } else if (item.getClickBusinessType() == GemLayerClickBusinessType.BizRouteTypePath) {
//            if (!ConvertUtils.isEmpty(mRouteLineInfos)) {
//                int index = -1;
//                for (int t = 0; t < mRouteLineInfos.size(); t++) {
//                    if (item.getIndex() == mRouteLineInfos.get(t).getMPathID()) {
//                        index = t;
//                    }
//                }
//                if (index != -1 && index != getCurrentIndex()) {
//                    mRoutePackage.selectRoute(MapType.MAIN_SCREEN_MAIN_MAP, index);
//                }
//            }
//        } else if (item.getClickBusinessType() == GemLayerClickBusinessType.BizRouteTypeTrafficEventTip) {
//            final PoiInfoEntity poiInfo = new PoiInfoEntity();
//            poiInfo.setPid(String.valueOf(item.getEventId()));
//            poiInfo.setPoint(new GeoPoint(item.getLog(), item.getLat(), item.getZ()));
//            final Bundle bundle = new Bundle();
//            bundle.putParcelable(AutoMapConstant.TrafficEventBundleKey.BUNDLE_KEY_ENTITY, poiInfo);
//            final TrafficEventFragment trafficEventFragment;
//            final BaseFragment fragment = StackManager.getInstance().getCurrentFragment(mViewModel.mScreenId);
//            if (fragment != null && fragment instanceof TrafficEventFragment) {
//                trafficEventFragment = (TrafficEventFragment) fragment;
//                trafficEventFragment.setArguments(bundle);
//                trafficEventFragment.onInitData();
//            } else {
//                trafficEventFragment = new TrafficEventFragment();
//                addFragment(trafficEventFragment, bundle);
//            }
//        } else if (item.getClickBusinessType() == GemLayerClickBusinessType.BizRouteTypeViaChargeStationPoint) {
//            if (mRouteChargeStationParam == null) {
//                return;
//            }
//            final ArrayList<RouteChargeStationInfo> routeChargeStationInfos = mRouteChargeStationParam.getMRouteChargeStationInfos();
//            if (routeChargeStationInfos == null) {
//                return;
//            }
//            final ArrayList<RouteChargeStationDetailInfo> routeChargeStationDetailInfo = routeChargeStationInfos.get(getCurrentIndex())
//                    .getMRouteChargeStationDetailInfo();
//            if (routeChargeStationDetailInfo == null || routeChargeStationDetailInfo.size() <= item.getIndex()) {
//                return;
//            }
//            final Fragment fragment = (Fragment) ARouter.getInstance().build(RoutePath.Route.ALTER_CHARGE_FRAGMENT).navigation();
//            final Bundle bundle = new Bundle();
//            bundle.putSerializable(AutoMapConstant.RouteBundleKey.BUNDLE_KEY_ALTER_CHARGE_STATION,
//                    routeChargeStationDetailInfo.get((int) item.getIndex()));
//            closeAllFragmentsUntilTargetFragment("AlterChargeFragment");
//            addFragment((BaseFragment) fragment, bundle);
//
//        }
//    }

/**
 * 服务区数据转POI数据
 * @param info 服务区数据
 * @return poi数据
 * */
private PoiInfoEntity getPoiEntryFromService(final RouteRestAreaDetailsInfo info) {
    final PoiInfoEntity poiInfoEntity = new PoiInfoEntity();
    poiInfoEntity.setName(info.getMServiceName());
    poiInfoEntity.setPid(info.getMServicePOIID());
    poiInfoEntity.setPoint(new GeoPoint(info.getMPos().getLon(), info.getMPos().getLat()));
    poiInfoEntity.setPointTypeCode("1083");
    poiInfoEntity.setPoiTag("服务区");
    poiInfoEntity.setDistance(TimeUtils.getInstance().getDistanceMsg(info.getMRemainDist()));
    poiInfoEntity.setPoint(new GeoPoint(info.getMPos().getLon(), info.getMPos().getLat()));
    return poiInfoEntity;
}

@Override
public void onImmersiveStatusChange(final MapType mapTypeId, final ImersiveStatus currentImersiveStatus) {
    if ((Objects.equals(NaviStatusPackage.getInstance().getCurrentNaviStatus()
            , NaviStatus.NaviStatusType.SELECT_ROUTE)) && currentImersiveStatus == ImersiveStatus.IMERSIVE) {
        Logger.i(TAG, "show route preview");
        mRoutePackage.showPreview(MapType.MAIN_SCREEN_MAIN_MAP);
    } else {
        if (!ConvertUtils.isEmpty(mViewModel)) {
            mViewModel.cancelTimer();
        }
    }
}

@Override
public void onSearchResult(final int taskId, final int errorCode, final String message, final SearchResultEntity searchResultEntity) {
    if (!ConvertUtils.isEmpty(mViewModel)) {
        mViewModel.hideSearchProgressUI();
    }
    if (ConvertUtils.isEmpty(searchResultEntity)) {
        return;
    }
    if (mLocalTaskId != taskId) {
        return;
    }
    if (searchResultEntity.getSearchType() == AutoMapConstant.SearchType.DEEP_INFO_SEARCH) {
        final PoiInfoEntity poiInfoEntity = searchResultEntity.getPoiList().get(0);
        if (!ConvertUtils.isEmpty(poiInfoEntity) && !ConvertUtils.isEmpty(mTaskMap.get(taskId))) {
            if (!ConvertUtils.isEmpty(mViewModel)) {
                mViewModel.setDetailsAddress(poiInfoEntity.getAddress());
            }
        }
    }
    Logger.i(TAG, GsonUtils.toJson(searchResultEntity.getPoiList()));
    if (searchResultEntity.getSearchType() == AutoMapConstant.SearchType.SEARCH_KEYWORD
            || searchResultEntity.getSearchType() == AutoMapConstant.SearchType.ALONG_WAY_SEARCH
            || searchResultEntity.getSearchType() == AutoMapConstant.SearchType.AROUND_SEARCH
            || searchResultEntity.getSearchType() == AutoMapConstant.SearchType.EN_ROUTE_KEYWORD_SEARCH) {
        if (searchResultEntity.getKeyword().equals(ResourceUtils.Companion.getInstance().getString(R.string.route_search_keyword_charge))) {
            mGasChargeAlongList.clear();
            final List<RouteParam> allPoiParamList = mRoutePackage.getAllPoiParamList(MapType.MAIN_SCREEN_MAIN_MAP);
            if (allPoiParamList.size() >= 2) {
                allPoiParamList.remove(0);
                allPoiParamList.remove(allPoiParamList.size() - 1);
            }
            mGasChargeAlongList.addAll(allPoiParamList);
            if (!ConvertUtils.isEmpty(mViewModel)) {
                mViewModel.showRouteSearchChargeListUI(searchResultEntity.getPoiList(), mGasChargeAlongList, mListSearchType);
            }
        }
    } else if (searchResultEntity.getSearchType() == AutoMapConstant.SearchType.LINE_DEEP_INFO_SEARCH
            || searchResultEntity.getSearchType() == AutoMapConstant.SearchType.POI_SEARCH) {
        final PoiInfoEntity poiInfoEntity = searchResultEntity.getPoiList().get(0);
        if (!ConvertUtils.isEmpty(poiInfoEntity) && !ConvertUtils.isEmpty(mTaskMap.get(taskId))) {
            if (!ConvertUtils.isEmpty(mViewModel)) {
                mViewModel.showRouteSearchDetailsUI(mTaskMap.get(taskId), poiInfoEntity, mGasChargeAlongList, mListSearchType);
            }
            getAddress(mTaskMap.get(taskId));
        }
    }
}

/**
 * 开始导航时埋点
 * */
@HookMethod(eventName = BuryConstant.EventName.AMAP_NAVI_START)
public void setPoint() {
    int index = -1;
    String label = "默认路线";
    if (!ConvertUtils.isEmpty(mRoutePackage.getSelectRouteIndex())
            && !ConvertUtils.isEmpty(mRoutePackage.getSelectRouteIndex().get(MapType.MAIN_SCREEN_MAIN_MAP))) {
        index = mRoutePackage.getSelectRouteIndex().get(MapType.MAIN_SCREEN_MAIN_MAP);
    }
    if (!ConvertUtils.isEmpty(mRouteLineInfos) && index != -1 && index < mRouteLineInfos.size()
            && !ConvertUtils.isEmpty(mRouteLineInfos.get(index))) {
        label = mRouteLineInfos.get(index).getMLabel();
    }
    BuryProperty property = new BuryProperty.Builder()
            .setParams(BuryConstant.ProperType.BURY_KEY_HOME_PREDICTION, Integer.toString(index))
            .setParams(BuryConstant.ProperType.BURY_KEY_ROUTE_PREFERENCE, label)
            .build();
    BuryPointController.getInstance().setBuryProps(property);
}
}

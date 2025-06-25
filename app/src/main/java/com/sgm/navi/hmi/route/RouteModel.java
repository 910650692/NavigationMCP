package com.sgm.navi.hmi.route;

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
import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.bean.BuryProperty;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.burypoint.controller.BuryPointController;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.limit.LimitCitySelectionFragment;
import com.sgm.navi.hmi.limit.LimitDriveFragment;
import com.sgm.navi.hmi.poi.PoiDetailsFragment;
import com.sgm.navi.hmi.search.parking.TerminalParkingFragment;
import com.sgm.navi.scene.RoutePath;
import com.sgm.navi.scene.impl.imersive.ImersiveStatus;
import com.sgm.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.layer.RouteLineLayerParam;
import com.sgm.navi.service.define.layer.refix.LayerItemRoutePointClickResult;
import com.sgm.navi.service.define.layer.refix.LayerPointItemType;
import com.sgm.navi.service.define.map.MapStateStyle;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.define.route.EvRangeOnRouteInfo;
import com.sgm.navi.service.define.route.RequestRouteResult;
import com.sgm.navi.service.define.route.RouteChargeStationParam;
import com.sgm.navi.service.define.route.RouteLineInfo;
import com.sgm.navi.service.define.route.RouteLineSegmentInfo;
import com.sgm.navi.service.define.route.RouteMsgPushInfo;
import com.sgm.navi.service.define.route.RouteParam;
import com.sgm.navi.service.define.route.RoutePoiType;
import com.sgm.navi.service.define.route.RoutePoint;
import com.sgm.navi.service.define.route.RoutePriorityType;
import com.sgm.navi.service.define.route.RouteRequestParam;
import com.sgm.navi.service.define.route.RouteRestAreaDetailsInfo;
import com.sgm.navi.service.define.route.RouteRestAreaInfo;
import com.sgm.navi.service.define.route.RouteRestAreaParam;
import com.sgm.navi.service.define.route.RouteRestirctionID;
import com.sgm.navi.service.define.route.RouteRestrictionInfo;
import com.sgm.navi.service.define.route.RouteRestrictionParam;
import com.sgm.navi.service.define.route.RouteSpeechRequestParam;
import com.sgm.navi.service.define.route.RouteSupplementInfo;
import com.sgm.navi.service.define.route.RouteSupplementParams;
import com.sgm.navi.service.define.route.RouteWayID;
import com.sgm.navi.service.define.route.RouteWeatherInfo;
import com.sgm.navi.service.define.route.RouteWeatherParam;
import com.sgm.navi.service.define.search.ETAInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.search.SearchResultEntity;
import com.sgm.navi.service.define.setting.SettingController;
import com.sgm.navi.service.define.utils.NumberUtils;
import com.sgm.navi.service.greendao.setting.SettingManager;
import com.sgm.navi.service.logicpaket.calibration.CalibrationPackage;
import com.sgm.navi.service.logicpaket.layer.ILayerPackageCallBack;
import com.sgm.navi.service.logicpaket.layer.LayerPackage;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.mapdata.MapDataPackage;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.sgm.navi.service.logicpaket.route.IRouteResultObserver;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.service.logicpaket.search.SearchResultCallback;
import com.sgm.navi.ui.base.BaseFragment;
import com.sgm.navi.ui.base.BaseModel;

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
    private boolean mChargeStationReady = false;
    private List<RouteRestrictionInfo> mRouteRestrictionInfo;
    private List<RouteParam> mRouteParams;
    private Map<Integer, PoiInfoEntity> mTaskMap = new HashMap<>();
    private long mRestirctionTaskId;
    private RouteRestrictionParam mRouteRestrictionParams;
    private RequestRouteResult mRequestRouteResults;
    private boolean mIsFirstRequest = true;
    private String mSuccessMsg = "";
    private int mCarType = -1;

    private List<RouteParam> mGasChargeAlongList = new ArrayList<>();
    private List<RouteParam> mRouteGasChargeAlongList = new ArrayList<>();

    private int mLocalTaskId = -1;
    //0:充电沿途 1：充电终点 2：充电周边 3：服务区
    private int mListSearchType;
    private int mParkSearchId = -1;
    private int mEndSearchId = -1;
    private List<RouteRestAreaInfo> mRouteRestAreaInfos;
    private ImersiveStatus mCurrentImersiveStatus;

    public RouteModel() {
        mLayerPackage = LayerPackage.getInstance();
        mRoutePackage = RoutePackage.getInstance();
        mSearchPackage = SearchPackage.getInstance();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        Logger.d(TAG, "registerCallBack");
        ImmersiveStatusScene.getInstance().registerCallback(TAG, this);
        mLayerPackage.registerCallBack(MapType.MAIN_SCREEN_MAIN_MAP, this);
        mSearchPackage.registerCallBack(TAG, this);
        mRoutePackage.registerRouteObserver(TAG, this);
        MapPackage.getInstance().setMapStateStyle(MapType.MAIN_SCREEN_MAIN_MAP, MapStateStyle.MAP_ROUTING);
    }

    @Override
    public void onStart() {
        super.onStart();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        Logger.d(TAG, "unRegisterCallBack");
        ImmersiveStatusScene.getInstance().unRegisterCallback(TAG);
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
        ThreadManager.getInstance().execute(() -> {
            param.setMMapTypeId(MapType.MAIN_SCREEN_MAIN_MAP);
            mRoutePackage.requestRoute(param);
        });
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
        ThreadManager.getInstance().execute(() -> mRoutePackage.requestChangeEnd(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity));
    }

    /**
     * 清除路线信息
     * */
    public void clearRouteLine() {
        mRoutePackage.clearRouteLine(MapType.MAIN_SCREEN_MAIN_MAP);
    }


    /**
     * 请求天气数据
     * */
    public void getWeatherList() {
        if (getCurrentIndex() == -1) {
            Logger.e(TAG, "天气请求失败");
            return;
        }
        ThreadManager.getInstance().execute(() -> mRoutePackage.requestRouteWeather(MapType.MAIN_SCREEN_MAIN_MAP, getCurrentIndex()));
        if (!ConvertUtils.isEmpty(mViewModel)) {
            mViewModel.showSearchProgressUI();
        }
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
        mListSearchType = 3;
        clearSearchLabel();
        ThreadManager.getInstance().execute(() -> mRoutePackage.requestRouteRestArea(getCurrentIndex()));
        if (!ConvertUtils.isEmpty(mViewModel)) {
            mViewModel.showSearchProgressUI();
        }
    }

    /**
     * 获取服务区列表
     * */
    public void setSearchListAndShow() {
        mListSearchType = 3;
        clearSearchLabel();
        if (ConvertUtils.isEmpty(mRouteRestAreaInfos)) {
            return;
        }
        final RouteRestAreaInfo routeRestAreaInfo = mRouteRestAreaInfos.get(0);
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
        ThreadManager.getInstance().postUi(() -> {
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
        });

    }

    /**
     * 请求充电站列表
     * @param keyWord  关键字
     * @param searchType 搜索方式
     * */
    public void getSearchListChargeAndShow(final String keyWord, final int searchType) {
        mListSearchType = searchType;
        clearSearchLabel();
        if (searchType == 0) {
            mLocalTaskId = mSearchPackage.enRouteKeywordSearch(keyWord);
        } else if (searchType == 1) {
            final RouteParam endPoint = mRoutePackage.getEndPoint(MapType.MAIN_SCREEN_MAIN_MAP);
            mLocalTaskId = mSearchPackage.aroundSearch(1, keyWord, new GeoPoint(endPoint.getRealPos().getLon(), endPoint.getRealPos().getLat()), false);
        } else {
            mLocalTaskId = mSearchPackage.aroundSearch(1, keyWord);
        }
        if (!ConvertUtils.isEmpty(mViewModel)) {
            mViewModel.showSearchProgressUI();
        }
    }

    /**
     * 请求详情数据
     * @param poiInfoEntity  请求参数
     * */
    public void getSearchDetailsMode(final PoiInfoEntity poiInfoEntity) {
        mTaskMap.clear();
        if (poiInfoEntity == null) {
            Logger.d(TAG, "getSearchDetailsMode poiInfoEntity is null");
            return;
        }
        ThreadManager.getInstance().execute(() -> {
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
            } else if (poiInfoEntity.getPoiTag().equals(ResourceUtils.Companion.getInstance().getString(R.string.route_search_keyword_gas))
                    || pointType == AutoMapConstant.PointTypeCode.GAS_STATION) {
                mLocalTaskId = mSearchPackage.poiIdSearch(poiInfoEntity.getPid());
            }
            mTaskMap.put(mLocalTaskId, poiInfoEntity);
        });
        if (!ConvertUtils.isEmpty(mViewModel)) {
            mViewModel.showSearchProgressUI();
        }
    }

    /**
     * 请求服务区详情地址
     * @param poiInfoEntity  请求参数
     * */
    public void getAddress(final PoiInfoEntity poiInfoEntity) {
        if (poiInfoEntity == null) {
            Logger.d(TAG, "getAddress is null");
            return;
        }
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
                boolean isSame = true;

                if (mGasChargeAlongList.size() != mRouteGasChargeAlongList.size()) {
                    isSame = false;
                } else {
                    for (RouteParam fristRouteParam : mGasChargeAlongList) {
                        boolean hasSame = false;
                        for (RouteParam secondRouteParam : mRouteGasChargeAlongList) {
                            if (isTheSamePoi(fristRouteParam, secondRouteParam)) {
                                hasSame = true;
                                break;
                            }
                        }
                        if (!hasSame) {
                            isSame = false;
                            break;
                        }
                    }
                }

                mViewModel.updateChareList(mGasChargeAlongList, mListSearchType, isSame);
            }
        } else {
            mRoutePackage.removeVia(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity, true);
        }
    }

    /**
     * 判断两个点是否一样
     * @param firstRouteParam  第一参数
     * @param secondRouteParam 第二个参数
     * */
    public boolean isTheSamePoi(final RouteParam firstRouteParam, final RouteParam secondRouteParam) {
        if (ConvertUtils.isEmpty(firstRouteParam) || ConvertUtils.isEmpty(secondRouteParam)
                || ConvertUtils.isEmpty(firstRouteParam.getRealPos())
                || ConvertUtils.isEmpty(secondRouteParam.getRealPos())) {
            return false;
        }
        return (!ConvertUtils.isEmpty(firstRouteParam.getPoiID()) && Objects.equals(firstRouteParam.getPoiID(), secondRouteParam.getPoiID()))
                || (firstRouteParam.getRealPos().getLat() == secondRouteParam.getRealPos().getLat()
                && firstRouteParam.getRealPos().getLon() == secondRouteParam.getRealPos().getLon());
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
                boolean isSame = true;

                if (mGasChargeAlongList.size() != mRouteGasChargeAlongList.size()) {
                    isSame = false;
                } else {
                    for (RouteParam fristRouteParam : mGasChargeAlongList) {
                        boolean hasSame = false;
                        for (RouteParam secondRouteParam : mRouteGasChargeAlongList) {
                            if (isTheSamePoi(fristRouteParam, secondRouteParam)) {
                                hasSame = true;
                                break;
                            }
                        }
                        if (!hasSame) {
                            isSame = false;
                            break;
                        }
                    }
                }

                mViewModel.updateChareList(mGasChargeAlongList, mListSearchType, isSame);
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
        if (!ConvertUtils.isEmpty(mViewModel)) {
            mViewModel.setTabSearchVisibility(false);
        }
    }

    /**
     * 获取当前点到此点的时间和距离
     * @param geoPoint 目标点
     * @return 时间和距离
     * */
    public CompletableFuture<ETAInfo> getTravelTimeFutureIncludeChargeLeft(final GeoPoint geoPoint) {
        return mSearchPackage.getTravelTimeFutureIncludeChargeLeft(geoPoint);
    }

    /**
     * 获取当前点到此点的时间和距离
     * @param startPoint 起点
     * @param endPoint 终点
     * @return 时间和距离
     * */
    public CompletableFuture<ETAInfo> getTravelTimeFutureIncludeChargeLeft(final GeoPoint startPoint, final GeoPoint endPoint) {
        return mSearchPackage.getTravelTimeFutureIncludeChargeLeft(startPoint, endPoint);
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
        addPoiDetailsFragment(new LimitDriveFragment(), bundle);
        closeAllFragmentsUntilTargetFragment(LimitCitySelectionFragment.class.getName());
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
        if (getCurrentIndex() == -1) {
            Logger.d(TAG, "Index out of bounds ");
            return;
        }
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
        mIsFirstRequest = false;
        mSuccessMsg = successMsg;
    }

    /***
     * 显示成功toast
     */
    public void showSuccessMsg() {
        if (!ConvertUtils.isEmpty(mSuccessMsg)) {
            ThreadManager.getInstance().postUi(() -> {
                ToastUtils.Companion.getInstance().showCustomToastView(mSuccessMsg);
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
        if (index == -1 || mRoutePackage.getEvRangeOnRouteInfos() == null
                || mRoutePackage.getEvRangeOnRouteInfos().isEmpty() || index >= mRoutePackage.getEvRangeOnRouteInfos().size()) {
            Logger.d(TAG, "Index out of bounds ");
            return null;
        }

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

    /***
     * 是否是启终点
     * @param poiInfoEntity poi数据
     * @return 是否在路径上
     */
    public boolean isStartOrEndRouteParam(final PoiInfoEntity poiInfoEntity) {
        return mRoutePackage.isStartOrEndRouteParam(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity);
    }

    /**
     * 获取选中的路线信息
     * @return 路线信息
     * */
    public RouteLineInfo getSelectLineInfo() {
        if (!ConvertUtils.isEmpty(mRouteLineInfos) && mRouteLineInfos.size() > getCurrentIndex()
                && getCurrentIndex() != -1) {
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

    public boolean isRouteTips() {
        return mRoutePackage.isRouteTips();
    }

    public void setRouteTips(boolean mRouteTips) {
        mRoutePackage.setRouteTips(mRouteTips);
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
        if (!mRoutePackage.isRouteState()) {
            return;
        }
        if (mRouteLineInfos.size() <= NumberUtils.NUM_0) {
            return;
        }
        if (!ConvertUtils.isEmpty(mViewModel)) {
            mViewModel.setRouteResultListUI(mRouteLineInfos);
        }
        showUIOnlineOffline(requestRouteResult.isMIsOnlineRoute());
        RouteParam endRouteParam = mRoutePackage.getEndPoint(requestRouteResult.getMMapTypeId());
        if (endRouteParam == null) {
            Logger.e(TAG, "error endParam");
            return;
        }
        ThreadManager.getInstance().postDelay( () -> {
            if (RoutePackage.getInstance().isRouteState()) {
                mEndSearchId = mSearchPackage.poiIdSearch(endRouteParam.getPoiID(), true);
            }
        }, 2000);
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

    /**
     * 动力类型标定
     * -1 无效值
     * 0 汽油车
     * 1 纯电动车
     * 2 插电式混动汽车
     * @return 动力类型
     */
    public int powerType() {
        if (mCarType == -1) {
            mCarType = CalibrationPackage.getInstance().powerType();
            Logger.d(TAG, "mCarType: " + mCarType);
        }
        //无效默认显示电车
        if (mCarType == -1) {
            return 1;
        }
        return mCarType;
    }

    @Override
    public void onRouteDrawLine(final RouteLineLayerParam routeLineLayerParam) {
        if (!ConvertUtils.isEmpty(mViewModel)) {
            mViewModel.hideProgressUI(true);
        }
        mRoutePackage.showRouteLine(routeLineLayerParam.getMMapTypeId());
        //todo 图层去设置全览
        ImmersiveStatusScene.getInstance().setImmersiveStatus(MapType.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.IMERSIVE);
        if (!mRoutePackage.isRouteState()) {
            return;
        }
        ThreadManager.getInstance().postDelay( () -> {
            if (RoutePackage.getInstance().isRouteState()) {
                final RoutePoint endPoint = routeLineLayerParam.getMRouteLinePoints().getMEndPoints().get(0);
                mParkSearchId = mSearchPackage.aroundSearch(1, BuryConstant.SearchType.PARKING, endPoint.getMPos(),"2000", true);
            }
        }, 2000);
    }


    @Override
    public void onRouteRestAreaInfo(final RouteRestAreaParam routeRestAreaParam) {
        if (ConvertUtils.isEmpty(routeRestAreaParam)) {
            return;
        }
        mRouteRestAreaInfos = routeRestAreaParam.getMRouteRestAreaInfos();
        setSearchListAndShow();
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
        int currentIndex = getCurrentIndex();
        if (ConvertUtils.isEmpty(mRouteRestrictionInfo)
                || currentIndex == -1
                || currentIndex >= mRouteRestrictionInfo.size()
                || ConvertUtils.isEmpty(mRouteRestrictionInfo.get(currentIndex))) {
            if (!ConvertUtils.isEmpty(mViewModel)) {
                mViewModel.updateRestrictionTextUI(RouteRestirctionID.REATIRCTION_LIMITTIPSTYPEINVALID);
            }
            return;
        }
        if (!ConvertUtils.isEmpty(mViewModel)) {
            mViewModel.updateRestrictionTextUI((int) mRouteRestrictionInfo.get(currentIndex).getMTitleType());
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
        addPoiDetailsFragment(new TerminalParkingFragment(), bundle);
    }

    /***
     * 清除终点停车场扎标
     */
    public void clearEndParkPoint() {
        mRoutePackage.clearEndParkPoint(MapType.MAIN_SCREEN_MAIN_MAP);
    }

    @Override
    public void onRouteAllRoutePoiInfo(final RequestRouteResult requestRouteResult) {
        mRouteParams = requestRouteResult.getMRouteParams();
        if (mRouteParams == null || mRouteParams.isEmpty()) {
            Logger.e(TAG, "routeParams is null");
            return;
        }
        mViewModel.getEndName().set(mRouteParams.get(mRouteParams.size() - NumberUtils.NUM_1).getName());
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
            mRoutePackage.setCarLogoVisible(MapType.MAIN_SCREEN_MAIN_MAP, true);
            NaviStatusPackage.getInstance().setNaviStatus(NaviStatus.NaviStatusType.NO_STATUS);
        }
    }

    @Override
    public void onRouteOffline(final MapType mapTypeId, final String errorMsg) {
        if (!ConvertUtils.isEmpty(errorMsg)) {
            ThreadManager.getInstance().postUi(() -> {
                ToastUtils.Companion.getInstance().showCustomToastView(errorMsg);
            });
        }
        if (!ConvertUtils.isEmpty(mViewModel)) {
            mViewModel.showOfflineProgressUI();
        }
    }

    @Override
    public void onRouteRequest() {
        mSearchPackage.clearLabelMark();
        mRoutePackage.setCarLogoVisible(MapType.MAIN_SCREEN_MAIN_MAP, false);
        clearWeatherView();
        clearRestArea();
        clearRestrictionView();
        mChargeStationReady = false;
        if (!ConvertUtils.isEmpty(mViewModel)) {
            mViewModel.showProgressUI();
        }
        if (!mRoutePackage.isRouteState()) {
            return;
        }
        if (!ConvertUtils.isEmpty(mViewModel)) {
            mViewModel.withoutSupplementPointsView();
            mViewModel.showNormalRouteUI(false);
        }
    }

    @Override
    public void onReroute() {
        if (!ConvertUtils.isEmpty(mViewModel)) {
            Logger.i(TAG, "偏航引发算路");
            mViewModel.showProgressUIOnly();
        }
    }

    @Override
    public void onRouteDetails(List<RouteLineSegmentInfo> routeLineDetail) {
        if (!ConvertUtils.isEmpty(mViewModel)) {
            Logger.i(TAG, "展示路线详情");
            mViewModel.showRouteDetails(routeLineDetail);
        }
    }

    @Override
    public void onReRouteError() {
        if (!ConvertUtils.isEmpty(mViewModel)) {
            Logger.i(TAG, "静默算路失败");
            mViewModel.hideProgressUI(false);
        }
    }

    @Override
    public void onRouteSlected(final MapType mapTypeId, final int routeIndex, boolean isFirst) {
        if (!mRoutePackage.isRouteState()) {
            return;
        }
        if (routeIndex == -1) {
            return;
        }
        if (mapTypeId == MapType.MAIN_SCREEN_MAIN_MAP) {
            if (!isFirst) {
                if (!ConvertUtils.isEmpty(mViewModel)) {
                    mViewModel.updateSelectRouteUI(routeIndex);
                    mViewModel.showNormalRouteUI(true);
                }
                mSearchPackage.clearLabelMark();
                mViewModel.cancelTimer();
                clearWeatherView();
                clearRestArea();
            }
            mRoutePackage.setRouteTips(false);
            if (!ConvertUtils.isEmpty(mViewModel) && mRouteLineInfos != null) {
                if (routeIndex >= 0 && routeIndex < mRouteLineInfos.size()
                        && NaviStatusPackage.getInstance().getCurrentNaviStatus().equals(NaviStatus.NaviStatusType.SELECT_ROUTE)) {
                    if (!mRouteLineInfos.get(routeIndex).isMCanBeArrive() && !mRouteLineInfos.get(routeIndex).isMRestoration()) {
                        mViewModel.showTripDialog(ResourceUtils.Companion.getInstance().getString(R.string.route_trip_title)
                                , ResourceUtils.Companion.getInstance().getString(R.string.route_trip_elec_not_arrive));
                        mRoutePackage.setRouteTips(true);
                    } else if (mRouteLineInfos.get(routeIndex).getMRemainPercent() < 20 && !mRouteLineInfos.get(routeIndex).isMRestoration()) {
                        mViewModel.showTripDialog(ResourceUtils.Companion.getInstance().getString(R.string.route_trip_title)
                                , ResourceUtils.Companion.getInstance().getString(R.string.route_trip_elec_small));
                        mRoutePackage.setRouteTips(true);
                    }
                    final boolean atLeastDistance = !ConvertUtils.isEmpty(mRouteLineInfos)
                            && mRouteLineInfos.size() > routeIndex
                            && !ConvertUtils.isEmpty(mRouteLineInfos.get(routeIndex))
                            && mRouteLineInfos.get(routeIndex).getMDistance() >= 50 * 1000;
                    if (!ConvertUtils.isEmpty(mViewModel)) {
                        mViewModel.showHideTab(atLeastDistance);
                    }
                    if (powerType() == 1) {
                        mViewModel.showHideElicCheckBox(mRouteLineInfos.get(routeIndex).getMRemainPercent() < 20
                                || mRouteLineInfos.get(routeIndex).isMChargingStation());
                    } else {
                        mViewModel.showHideElicCheckBox(false);
                    }
                }
            }

            //TODO CR
            if (mViewModel != null && mViewModel.isNDCar()) {
                if (!ConvertUtils.isEmpty(mViewModel) && mRouteChargeStationParam != null
                        && mRouteChargeStationParam.getMRouteSupplementParams() != null
                        && !mRouteChargeStationParam.getMRouteSupplementParams().isEmpty()
                        && routeIndex < mRouteChargeStationParam.getMRouteSupplementParams().size()
                        && mChargeStationReady) {
                    final RouteSupplementParams routeSupplementParams = mRouteChargeStationParam.getMRouteSupplementParams().get(routeIndex);
                    mViewModel.updateSupplementPointsView(routeSupplementParams.getMRouteSupplementInfos()
                            , routeSupplementParams.getMTotalDistance());
                }
            }
        }
    }

    @Override
    public void onRouteChargeStationInfo(final RouteChargeStationParam routeChargeStationParam) {
        mRouteChargeStationParam = routeChargeStationParam;
        if (routeChargeStationParam != null) {
             mRoutePackage.updateRouteChargeStation(MapType.MAIN_SCREEN_MAIN_MAP, routeChargeStationParam);
        }

        if (!mRoutePackage.isRouteState()) {
            return;
        }

        //TODO CR
        if (mViewModel != null && mViewModel.isNDCar()) {
            if (!ConvertUtils.isEmpty(mViewModel) && mRouteChargeStationParam != null
                    && mRouteChargeStationParam.getMRouteSupplementParams() != null
                    && !mRouteChargeStationParam.getMRouteSupplementParams().isEmpty()
                    && getCurrentIndex() < mRouteChargeStationParam.getMRouteSupplementParams().size()) {
                mChargeStationReady = true;
                final RouteSupplementParams routeSupplementParams = mRouteChargeStationParam.getMRouteSupplementParams().get(getCurrentIndex());
                mViewModel.updateSupplementPointsView(routeSupplementParams.getMRouteSupplementInfos()
                        , routeSupplementParams.getMTotalDistance());
            } else {
                if (!ConvertUtils.isEmpty(mViewModel)) {
                    mViewModel.withoutSupplementPointsView();
                }
            }
        }
    }

    @Override
    public void onRouteItemClick(final MapType mapTypeId, final LayerPointItemType type, final LayerItemRoutePointClickResult item) {
        Logger.e(TAG, "onRouteItemClick-RouteModel");
        if (ConvertUtils.isEmpty(item)) {
            return;
        }
        if (!NaviStatus.NaviStatusType.SELECT_ROUTE.equals(NaviStatusPackage.getInstance().getCurrentNaviStatus())) {
            Logger.i(TAG, "is not on route page");
            return;
        }
        switch (type) {
            case ROUTE_PATH:
                ThreadManager.getInstance().execute(() -> {
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
                });
                break;
            case ROUTE_GUIDE_LABEL:
                ThreadManager.getInstance().execute(() -> {
                    if (!ConvertUtils.isEmpty(mRouteLineInfos)) {
                        int index = -1;
                        for (int t = 0; t < mRouteLineInfos.size(); t++) {
                            if (item.getEventID() == mRouteLineInfos.get(t).getMPathID()) {
                                index = t;
                            }
                        }

                        if (index != -1 && index != getCurrentIndex()) {
                            mRoutePackage.selectRoute(MapType.MAIN_SCREEN_MAIN_MAP, index);
                        }
                    }
                });
                break;
            case ROUTE_POINT_START:
            case ROUTE_POINT_END:
            case ROUTE_POINT_VIA:
            case ROUTE_POINT_VIA_CHARGE:
                final PoiInfoEntity poiInfo = new PoiInfoEntity();
                poiInfo.setPoint(new GeoPoint(item.getLog(), item.getLat()));
                final Bundle poiBundle = new Bundle();
                poiBundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_DETAIL, poiInfo);
                poiBundle.putInt(AutoMapConstant.PoiBundleKey.BUNDLE_KEY_START_POI_TYPE, AutoMapConstant.PoiType.POI_MAP_CLICK);
                final PoiDetailsFragment poiFragment = new PoiDetailsFragment();
                addPoiDetailsFragment(poiFragment, poiBundle);
                break;
            case ROUTE_POINT_WEATHER:
                if (!ConvertUtils.isEmpty(mRouteWeatherInfos) && mRouteWeatherInfos.size() > item.getIndex()) {
                    if (!ConvertUtils.isEmpty(mViewModel)) {
                        mViewModel.showWeatherDetailsUI(mRouteWeatherInfos.get((int) (item.getIndex())));
                    }
                }
                break;
            case ROUTE_POINT_REST_AREA:
                if (!ConvertUtils.isEmpty(mRouteRestAreaInfos) && mRouteRestAreaInfos.size() > getCurrentIndex()
                        && getCurrentIndex() != -1
                        && !ConvertUtils.isEmpty(mRouteRestAreaInfos.get(getCurrentIndex()))
                        && !ConvertUtils.isEmpty(mRouteRestAreaInfos.get(getCurrentIndex()).getMRouteRestAreaDetailsInfos())) {
                    final PoiInfoEntity poiEntryFromService = getPoiEntryFromService(
                            mRouteRestAreaInfos.get(getCurrentIndex()).getMRouteRestAreaDetailsInfos().get((int) (item.getIndex())));
                    getSearchDetailsMode(poiEntryFromService);
                }
                break;
            case ROUTE_POINT_VIA_CHARGE_STATION:
                int index = getCurrentIndex();
                if (mRouteChargeStationParam == null) {
                    return;
                }
                final ArrayList<RouteSupplementParams> RouteSupplementParams = mRouteChargeStationParam.getMRouteSupplementParams();
                if (RouteSupplementParams == null || index == -1 || RouteSupplementParams.isEmpty()
                || index >= RouteSupplementParams.size()) {
                    return;
                }
                final ArrayList<RouteSupplementInfo> routeSupplementInfos = RouteSupplementParams.get(index)
                        .getMRouteSupplementInfos();
                if (routeSupplementInfos == null || routeSupplementInfos.size() <= item.getIndex() || item.getIndex() == -1) {
                    return;
                }

                //TODO CR
                if (mViewModel != null && mViewModel.isNDCar()) {
                    final Fragment chargeFragment = (Fragment) ARouter.getInstance().build(RoutePath.Route.NEW_ALTER_CHARGE_FRAGMENT).navigation();
                    final Bundle chargeBundle = new Bundle();
                    chargeBundle.putSerializable(AutoMapConstant.RouteBundleKey.BUNDLE_KEY_ALTER_CHARGE_STATION,
                            routeSupplementInfos.get((int) item.getIndex()));
                    chargeBundle.putSerializable(AutoMapConstant.RouteBundleKey.BUNDLE_KEY_SUPPLEMENT,
                            mRouteChargeStationParam.getMRouteSupplementParams().get(index));
                    closeAllFragmentsUntilTargetFragment("NewAlterChargeFragment");
                    addFragment((BaseFragment) chargeFragment, chargeBundle);
                } else {
                    final Fragment chargeFragment = (Fragment) ARouter.getInstance().build(RoutePath.Route.ALTER_CHARGE_FRAGMENT).navigation();
                    final Bundle chargeBundle = new Bundle();
                    chargeBundle.putSerializable(AutoMapConstant.RouteBundleKey.BUNDLE_KEY_ALTER_CHARGE_STATION,
                            routeSupplementInfos.get((int) item.getIndex()));
                    closeAllFragmentsUntilTargetFragment("AlterChargeFragment");
                    addFragment((BaseFragment) chargeFragment, chargeBundle);
                }

                break;
            case ROUTE_POINT_END_PARK:
                clearEndParkPoint();
                showParkingFragment();
                break;
            default:
                break;
    }
}

/**
 * 跳转至补能规划界面
 * */
public void jumpToSupplementPlan() {
    if (mRouteChargeStationParam == null) {
        return;
    }
    final ArrayList<RouteSupplementParams> routeSupplementParams = mRouteChargeStationParam.getMRouteSupplementParams();
    if (routeSupplementParams == null || getCurrentIndex() == -1 || routeSupplementParams.isEmpty()
            || getCurrentIndex() >= routeSupplementParams.size()) {
        return;
    }
    final RouteSupplementParams routeSupplementParam = routeSupplementParams.get(getCurrentIndex());
    if (routeSupplementParam == null ) {
        return;
    }
    final Fragment chargeFragment = (Fragment) ARouter.getInstance().build(RoutePath.Route.NEW_ALTER_CHARGE_FRAGMENT).navigation();
    final Bundle chargeBundle = new Bundle();
    chargeBundle.putSerializable(AutoMapConstant.RouteBundleKey.BUNDLE_KEY_SUPPLEMENT, routeSupplementParam);
    addPoiDetailsFragment((BaseFragment) chargeFragment, chargeBundle);
}

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

/**
 * 绘制终点附近有可用停车点
 *
 * @param mapTypeId 屏幕ID
 */
private void showRoutePark(final MapType mapTypeId) {
    mRoutePackage.showRoutePark(mapTypeId);
}

@Override
public void onImmersiveStatusChange(final MapType mapTypeId, final ImersiveStatus currentImersiveStatus) {
    if (!Objects.equals(NaviStatusPackage.getInstance().getCurrentNaviStatus(), NaviStatus.NaviStatusType.SELECT_ROUTE)) {
        Logger.d(TAG, "不在选路态");
        return;
    }
    if (currentImersiveStatus == ImersiveStatus.IMERSIVE) {
        if (mCurrentImersiveStatus != currentImersiveStatus) {
            mRoutePackage.showPreview(MapType.MAIN_SCREEN_MAIN_MAP);
        }
    } else {
        if (!ConvertUtils.isEmpty(mViewModel)) {
            mViewModel.cancelTimer();
        }
    }
    mCurrentImersiveStatus = currentImersiveStatus;
}

    @Override
    public void onSilentSearchResult(final int taskId, final int errorCode, final String message, final SearchResultEntity searchResultEntity) {
        if (!RoutePackage.getInstance().isRouteState()) {
            mParkSearchId = -1;
            mEndSearchId = -1;
            return;
        }
        if (mParkSearchId == taskId) {
            if (searchResultEntity.getPoiList() != null && !searchResultEntity.getPoiList().isEmpty()
                    && mRoutePackage.isRouteState()) {
                PoiInfoEntity endPoiEntity = mRoutePackage.getEndEntity(MapType.MAIN_SCREEN_MAIN_MAP);
                if (endPoiEntity != null && endPoiEntity.getPointTypeCode() != null && endPoiEntity.getPointTypeCode().startsWith("1509")) {
                    Logger.d(TAG, "The endpoint is the parking");
                    return;
                }
                showRoutePark(MapType.MAIN_SCREEN_MAIN_MAP);
            }
        }
        if (mEndSearchId == taskId) {
            if (searchResultEntity.getPoiList() != null && !searchResultEntity.getPoiList().isEmpty()) {
                mRoutePackage.setEndEntity(MapType.MAIN_SCREEN_MAIN_MAP, searchResultEntity.getPoiList().get(0));
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
    List<PoiInfoEntity> poiList = searchResultEntity.getPoiList();
    if (poiList == null || poiList.isEmpty()) {
        Logger.d(TAG, "poiList is empty");
        return;
    }
    if (searchResultEntity.getSearchType() == AutoMapConstant.SearchType.DEEP_INFO_SEARCH) {
        final PoiInfoEntity poiInfoEntity = poiList.get(0);
        if (!ConvertUtils.isEmpty(poiInfoEntity) && !ConvertUtils.isEmpty(mTaskMap.get(taskId))) {
            if (!ConvertUtils.isEmpty(mViewModel)) {
                mViewModel.setDetailsAddress(poiInfoEntity.getAddress());
            }
        }
    }
    if (searchResultEntity.getSearchType() == AutoMapConstant.SearchType.SEARCH_KEYWORD
            || searchResultEntity.getSearchType() == AutoMapConstant.SearchType.ALONG_WAY_SEARCH
            || searchResultEntity.getSearchType() == AutoMapConstant.SearchType.AROUND_SEARCH
            || searchResultEntity.getSearchType() == AutoMapConstant.SearchType.EN_ROUTE_KEYWORD_SEARCH) {
        if (searchResultEntity.getKeyword().equals(ResourceUtils.Companion.getInstance().getString(R.string.route_search_keyword_charge))) {
            mGasChargeAlongList.clear();
            mRouteGasChargeAlongList.clear();
            final List<RouteParam> allPoiParamList = mRoutePackage.getAllPoiParamList(MapType.MAIN_SCREEN_MAIN_MAP);
            if (allPoiParamList.size() >= 2) {
                allPoiParamList.remove(0);
                allPoiParamList.remove(allPoiParamList.size() - 1);
            }
            mGasChargeAlongList.addAll(allPoiParamList);
            mRouteGasChargeAlongList.addAll(allPoiParamList);
            if (!ConvertUtils.isEmpty(mViewModel)) {
                mViewModel.showRouteSearchChargeListUI(searchResultEntity, mGasChargeAlongList, mListSearchType, 0);
            }
        }
        if (searchResultEntity.getKeyword().equals(ResourceUtils.Companion.getInstance().getString(R.string.route_search_keyword_gas))) {
            mGasChargeAlongList.clear();
            mRouteGasChargeAlongList.clear();
            final List<RouteParam> allPoiParamList = mRoutePackage.getAllPoiParamList(MapType.MAIN_SCREEN_MAIN_MAP);
            if (allPoiParamList.size() >= 2) {
                allPoiParamList.remove(0);
                allPoiParamList.remove(allPoiParamList.size() - 1);
            }
            mGasChargeAlongList.addAll(allPoiParamList);
            mRouteGasChargeAlongList.addAll(allPoiParamList);
            if (!ConvertUtils.isEmpty(mViewModel)) {
                mViewModel.showRouteSearchChargeListUI(searchResultEntity, mGasChargeAlongList, mListSearchType, 1);
            }
        }
    } else if (searchResultEntity.getSearchType() == AutoMapConstant.SearchType.LINE_DEEP_INFO_SEARCH
            || searchResultEntity.getSearchType() == AutoMapConstant.SearchType.POI_SEARCH) {
        final PoiInfoEntity poiInfoEntity = poiList.get(0);
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
    final BuryProperty property = new BuryProperty.Builder()
            .setParams(BuryConstant.ProperType.BURY_KEY_HOME_PREDICTION, Integer.toString(index + 1))
            .setParams(BuryConstant.ProperType.BURY_KEY_ROUTE_PREFERENCE, label)
            .build();
    BuryPointController.getInstance().setBuryProps(property);
}

    public void onReStoreFragment() {
        if (!ConvertUtils.isEmpty(mRequestRouteResults)) {
            onRouteResult(mRequestRouteResults);

            if (mRouteChargeStationParam != null) {
                int routeIndex = getCurrentIndex();
                ArrayList<RouteSupplementParams> routeSupplementParams = mRouteChargeStationParam.getMRouteSupplementParams();
                if (routeIndex != -1 && routeIndex < routeSupplementParams.size()) {
                    final RouteSupplementParams routeSupplementParam = routeSupplementParams.get(routeIndex);
                    mViewModel.updateSupplementPointsView(routeSupplementParam.getMRouteSupplementInfos()
                            , routeSupplementParam.getMTotalDistance());
                }
            }
//            mRoutePackage.selectRoute(MapType.MAIN_SCREEN_MAIN_MAP, getCurrentIndex());
//            if (!ConvertUtils.isEmpty(mRequestRouteResults.getMRouteRestAreaParam())) {
//                onRouteRestAreaInfo(mRequestRouteResults.getMRouteRestAreaParam());
//            }
        }
    }

    public void requestRouteDetails(int index) {
        ThreadManager.getInstance().execute(() -> mRoutePackage.requestRouteDetails(index));
    }
}

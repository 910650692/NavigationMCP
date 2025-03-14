package com.fy.navi.hmi.route;

import android.os.Bundle;
import android.util.Pair;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.limit.LimitDriveFragment;
import com.fy.navi.hmi.search.parking.TerminalParkingFragment;
import com.fy.navi.hmi.traffic.TrafficEventFragment;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.layer.GemBaseLayer;
import com.fy.navi.service.define.layer.GemLayerClickBusinessType;
import com.fy.navi.service.define.layer.GemLayerItem;
import com.fy.navi.service.define.layer.LayerType;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.map.MapStateStyle;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.map.MapTypeManager;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.route.EvRangeOnRouteInfo;
import com.fy.navi.service.define.route.RequestRouteResult;
import com.fy.navi.service.define.route.RouteAlongCityInfo;
import com.fy.navi.service.define.route.RouteAlongCityParam;
import com.fy.navi.service.define.route.RouteAlterChargeStationParam;
import com.fy.navi.service.define.route.RouteChargeStationInfo;
import com.fy.navi.service.define.route.RouteChargeStationParam;
import com.fy.navi.service.define.route.RouteLineInfo;
import com.fy.navi.service.define.route.RouteLineSegmentInfo;
import com.fy.navi.service.define.route.RouteMsgPushInfo;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.route.RoutePoiType;
import com.fy.navi.service.define.route.RouteRestAreaInfo;
import com.fy.navi.service.define.route.RouteRestAreaParam;
import com.fy.navi.service.define.route.RouteRestTollGateInfo;
import com.fy.navi.service.define.route.RouteRestTollGateParam;
import com.fy.navi.service.define.route.RouteRestirctionID;
import com.fy.navi.service.define.route.RouteRestrictionInfo;
import com.fy.navi.service.define.route.RouteRestrictionParam;
import com.fy.navi.service.define.route.RouteSpeechRequestParam;
import com.fy.navi.service.define.route.RouteTrafficIncidentInfo;
import com.fy.navi.service.define.route.RouteTrafficIncidentParam;
import com.fy.navi.service.define.route.RouteWayID;
import com.fy.navi.service.define.route.RouteWeatherInfo;
import com.fy.navi.service.define.route.RouteWeatherParam;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.define.setting.SettingController;
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

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public class RouteModel extends BaseModel<RouteViewModel> implements IRouteResultObserver, ILayerPackageCallBack, ImmersiveStatusScene.IImmersiveStatusCallBack, SearchResultCallback {
    private static final String TAG = RouteModel.class.getSimpleName();
    private final SearchPackage searchPackage;
    private RoutePackage routePackage;
    private LayerPackage mLayerPackage;
    private List<RouteLineInfo> routeLineInfos;
    private List<RouteWeatherInfo> routeWeatherInfos;
    private List<RouteRestrictionInfo> routeRestrictionInfo;
    private List<RouteAlongCityInfo> routeAlongCityInfos;
    private List<RouteParam> routeParams;
    private Map<Integer, PoiInfoEntity> taskMap = new HashMap<>();
    private long restirctionTaskId;
    private long alterChargeStationTaskId;
    private RouteRestrictionParam routeRestrictionParams;
    private RequestRouteResult requestRouteResults;

    private List<RouteParam> gasChargeAlongList = new ArrayList<>();

    int localTaskId = -1;
    private int listSearchType;
    private boolean isFirstRequest = true;

    public RouteModel() {
        mLayerPackage = LayerPackage.getInstance();
        routePackage = RoutePackage.getInstance();
        routePackage.registerRouteObserver("RouteModel", this);
        searchPackage = SearchPackage.getInstance();
        MapPackage.getInstance().setMapStateStyle(MapTypeId.MAIN_SCREEN_MAIN_MAP, MapStateStyle.MAP_ROUTING);
    }

    @Override
    public void onCreate() {
        super.onCreate();
    }

    @Override
    public void onStart() {
        super.onStart();
        Logger.d(TAG, "registerCallBack");
        ImmersiveStatusScene.getInstance().registerCallback("RouteModel", this);
        mLayerPackage.registerCallBack(MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId), this, LayerType.ROUTE_LAYER);
        searchPackage.registerCallBack("RouteModel",this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        Logger.d(TAG, "unRegisterCallBack");
        routePackage.clearRestrictionView(MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId));
        mLayerPackage.unRegisterCallBack(MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId), this, LayerType.ROUTE_LAYER);
        searchPackage.unRegisterCallBack("RouteModel");
    }
    public void requestRouteMode(PoiInfoEntity poiInfoEntity, int poiType, int routeWay) {
        routePackage.requestRoute(MapTypeId.MAIN_SCREEN_MAIN_MAP, poiInfoEntity, poiType, false, routeWay);
    }
    public void requestRouteRestoration(RouteMsgPushInfo routeMsgPushInfo) {
        routePackage.requestRouteRestoration(routeMsgPushInfo, MapTypeId.MAIN_SCREEN_MAIN_MAP);
    }
    public void requestRouteFromSpeech(RouteSpeechRequestParam param) {
        routePackage.requestRouteFromSpeech(param);
    }

    public void clearRouteLine() {
        routePackage.clearRouteLine(MapTypeId.MAIN_SCREEN_MAIN_MAP);
    }

    public List<RouteLineSegmentInfo> getDetailsResult(int index) {
        if (ConvertUtils.isEmpty(routeLineInfos) || routeLineInfos.isEmpty()) {
            return new ArrayList<>();
        }
        return routeLineInfos.get(index).getRouteLineSegmentInfos();
    }
    public void getWeatherList() {
        mViewModel.showSearchProgressUI();
        routePackage.requestRouteWeather(MapTypeId.MAIN_SCREEN_MAIN_MAP, routePackage.getSelectRouteIndex().get(MapTypeId.MAIN_SCREEN_MAIN_MAP));
    }

    public void hideWeatherList() {
        routePackage.clearWeatherView(MapTypeId.MAIN_SCREEN_MAIN_MAP);
        //hide weather list 不需要
        mViewModel.hideWeatherDeatilsUI();
    }
    public void getSearchListAndShow(String keyWord, int searchType) {
        listSearchType = searchType;
        clearSearchLabel();
        mViewModel.showSearchProgressUI();
        localTaskId = searchPackage.enRouteKeywordSearch(keyWord);
    }

    public void getSearchListChargeAndShow(String keyWord, int searchType) {
        listSearchType = searchType;
        clearSearchLabel();
        mViewModel.showSearchProgressUI();
        if (searchType == 0) {
            localTaskId = searchPackage.enRouteKeywordSearch(keyWord);
        } else if (searchType == 1) {
            RouteParam endPoint = routePackage.getEndPoint(MapTypeId.MAIN_SCREEN_MAIN_MAP);
            localTaskId = searchPackage.aroundSearch(1, keyWord, new GeoPoint(endPoint.getRealPos().lon, endPoint.getRealPos().lat));
        } else {
            localTaskId = searchPackage.aroundSearch(1, keyWord);
        }

    }
    public void getSearchDetailsMode(PoiInfoEntity poiInfoEntity) {
        taskMap.clear();
        mViewModel.showSearchProgressUI();
        int pointType = searchPackage.getPointTypeCode(poiInfoEntity.getPointTypeCode());
        if (poiInfoEntity.getPoiTag().equals(ResourceUtils.Companion.getInstance().getString(R.string.route_search_keyword_service))
                || pointType == AutoMapConstant.PointTypeCode.SERVICE_AREA) {
            List<String> poiIds = new ArrayList<>();
            poiIds.add(poiInfoEntity.getPid());
            localTaskId = searchPackage.doLineDeepInfoSearch(ResourceUtils.Companion.getInstance()
                    .getString(R.string.route_search_keyword_service), poiIds);
        } else if (poiInfoEntity.getPoiTag().equals(ResourceUtils.Companion.getInstance().getString(R.string.route_search_keyword_charge))
                || pointType == AutoMapConstant.PointTypeCode.CHARGING_STATION) {
            localTaskId = searchPackage.poiIdSearch(poiInfoEntity.getPid());
        }
        taskMap.put(localTaskId, poiInfoEntity);
    }

    public void gasChargeRemoveMode(PoiInfoEntity poiInfoEntity) {
        if (listSearchType == 0) {
            if (routePackage.isStartOrEndRouteParam(MapTypeId.MAIN_SCREEN_MAIN_MAP, poiInfoEntity)) {
                ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.route_error_add_start_end));
                return;
            }
            RouteParam routeParams = new RouteParam();
            for (RouteParam routeParam : gasChargeAlongList) {
                if (routePackage.isTheSamePoi(routeParam, poiInfoEntity)) {
                    routeParams = routeParam;
                    break;
                }
            }
            gasChargeAlongList.remove(routeParams);
            mViewModel.updateChareList(gasChargeAlongList, listSearchType);
        } else {
            routePackage.removeVia(MapTypeId.MAIN_SCREEN_MAIN_MAP, poiInfoEntity, false);
        }
    }

    public void gasChargeAddMode(PoiInfoEntity poiInfoEntity) {
        if (listSearchType == 0) {
            gasChargeAlongList.add(routePackage.getRouteParam(poiInfoEntity, RoutePoiType.ROUTE_POI_TYPE_WAY));
            mViewModel.updateChareList(gasChargeAlongList, listSearchType);
        } else {
            routePackage.addViaPoint(MapTypeId.MAIN_SCREEN_MAIN_MAP, poiInfoEntity, RoutePoiType.ROUTE_POI_TYPE_WAY);
        }
    }

    public void startAllRequest() {
        if (gasChargeAlongList.size() > 5) {
            ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.route_error_via_max));
            return;
        }
        routePackage.requestManyVia(MapTypeId.MAIN_SCREEN_MAIN_MAP, gasChargeAlongList);
    }

    public CompletableFuture<Pair<String, String>> getTravelTimeFuture(GeoPoint geoPoint) {
        return searchPackage.getTravelTimeFuture(geoPoint);
    }

    public void showRestrictionFragment() {
        if (ConvertUtils.isEmpty(routeRestrictionParams)) return;
        Bundle bundle = new Bundle();
        bundle.putSerializable(AutoMapConstant.CommonBundleKey.BUNDLE_KEY_LIMIT_ROUND, routeRestrictionParams);
        addFragment(new LimitDriveFragment(), bundle);
    }

    public void clearRestrictionView() {
        routePackage.clearRestrictionView(MapTypeId.MAIN_SCREEN_MAIN_MAP);
    }

    public void clearSearchLabel() {
        searchPackage.clearLabelMark();
    }

    public void clearWeatherView() {
        routePackage.clearWeatherView(MapTypeId.MAIN_SCREEN_MAIN_MAP);
    }

    public void changeParamListMode(int currentPosition, int movePosition) {
        if (routeParams.size() < 2) return;
        if (currentPosition < 0) return;
        if (movePosition < 0) return;
        if (ConvertUtils.isEmpty(routeParams)) return;
        if (routeParams.size() <= currentPosition || routeParams.size() <= movePosition) return;
        Collections.swap(routeParams, currentPosition, movePosition);
        routePackage.updateViaParamList(MapTypeId.MAIN_SCREEN_MAIN_MAP, routeParams);
    }

    public void deleteViaParamMode(int index) {
        if (ConvertUtils.isEmpty(routeParams)) return;
        if (routeParams.size() <= index) return;
        routeParams.remove(index);
        routePackage.updateViaParamList(MapTypeId.MAIN_SCREEN_MAIN_MAP, routeParams);
        requestRouteMode(null, -1, RouteWayID.ROUTE_WAY_DELETE_VIA);
    }

    public void addViaList(PoiInfoEntity poiInfoEntity) {
        routePackage.addViaPoint(MapTypeId.MAIN_SCREEN_MAIN_MAP, poiInfoEntity, RoutePoiType.ROUTE_POI_TYPE_WAY);
    }

    public void deleteViaParamMode(PoiInfoEntity poiInfoEntity) {
        routePackage.removeVia(MapTypeId.MAIN_SCREEN_MAIN_MAP, poiInfoEntity, true);
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
    public void onRouteSuccess() {
        routeLineInfos = null;
        routeWeatherInfos = null;
        routeRestrictionInfo = null;
        routeAlongCityInfos = null;
        mViewModel.hideProgressUI();
    }

    public void selectRoute(int index) {
        if (ConvertUtils.isEmpty(routeRestrictionInfo)) {
            mViewModel.updateRestrictionTextUI(-1);
            return;
        }
        mViewModel.updateRestrictionTextUI((int)routeRestrictionInfo.get(index).titleType);
    }

    public EvRangeOnRouteInfo getRangeOnRouteInfo(int index) {
        return routePackage.getEvRangeOnRouteInfos().get(index);
    }

    public boolean isBelongRouteParam(PoiInfoEntity poiInfoEntity) {
        return routePackage.isBelongRouteParam(MapTypeId.MAIN_SCREEN_MAIN_MAP, poiInfoEntity);
    }

    @Override
    public void onRouteResult(RequestRouteResult requestRouteResult) {
        if (ConvertUtils.isEmpty(requestRouteResult)) return;
        requestRouteResults = requestRouteResult;
        routeLineInfos = requestRouteResult.getRouteLineInfos();
        if (routeLineInfos.size() <= NumberUtils.NUM_0) return;
        mViewModel.setRouteResultListUI(routeLineInfos);
        showUIOnlineOffline(requestRouteResult.isOnlineRoute());
    }

    private void showUIOnlineOffline(boolean onlineRoute) {
        //离线
        if (Boolean.FALSE.equals(onlineRoute)) {
            //限行
            mViewModel.updateRestrictionTextUI(RouteRestirctionID.REATIRCTION_LIMITTIPSTYPENETWORK);
        }
    }

    @Override
    public void onRouteDrawLine(RouteLineLayerParam routeLineLayerParam) {
        routePackage.showRouteLine(routeLineLayerParam.getMapTypeId());
        routePackage.selectRoute(routeLineLayerParam.getMapTypeId(), NumberUtils.NUM_0);
        routePackage.showPreview(routeLineLayerParam.getMapTypeId());
    }

    @Override
    public void onRouteRestAreaInfo(RouteRestAreaParam routeRestAreaParam) {
        if (ConvertUtils.isEmpty(routeRestAreaParam)) return;
    }

    @Override
    public void onRouteWeatherInfo(RouteWeatherParam routeWeatherParam) {
        mViewModel.hideSearchProgressUI();
        if (ConvertUtils.isEmpty(routeWeatherParam)) {
            ThreadManager.getInstance().postUi(() -> ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.route_error_wearher)));
            return;
        }
        routeWeatherInfos = routeWeatherParam.getRouteWeatherInfos();
        if (!ConvertUtils.isEmpty(routeWeatherInfos) && routeWeatherInfos.size() > 0) {
            for (RouteWeatherInfo info : routeWeatherInfos) {
                info.mCityName = MapDataPackage.getInstance().getCityInfo(info.mCityID).name;
            }
        }
    }

    @Override
    public void onRouteRestrictionInfo(RouteRestrictionParam routeRestrictionParam) {
        restirctionTaskId = routePackage.requestRestirction(routeRestrictionParam.getMapTypeId());
        routeRestrictionInfo = routeRestrictionParam.getRouteRestrictionInfo();
        if (ConvertUtils.isEmpty(routeRestrictionInfo)) {
            mViewModel.updateRestrictionTextUI(RouteRestirctionID.REATIRCTION_LIMITTIPSTYPEINVALID);
            return;
        }
        mViewModel.updateRestrictionTextUI((int)routeRestrictionInfo.get(routePackage.getSelectRouteIndex().get(MapTypeId.MAIN_SCREEN_MAIN_MAP)).titleType);
    }

    @Override
    public void onRouteDrawReStrictedAreaInfo(RouteRestrictionParam routeRestrictionParam) {
        if (ConvertUtils.isEmpty(routeRestrictionParam.getRestrictedArea())) return;
        if (routeRestrictionParam.getRestrictedArea().getRequestId() == restirctionTaskId) {
            routePackage.clearRestrictionView(MapTypeId.MAIN_SCREEN_MAIN_MAP);
            routePackage.showRestrictionView(MapTypeId.MAIN_SCREEN_MAIN_MAP, routeRestrictionParam.getGReStrictedAreaResponseParam());
            routeRestrictionParams = routeRestrictionParam.copy();
        }
    }

    @Override
    public void onRouteRestTollGateInfo(RouteRestTollGateParam routeRestTollGateParam) {
    }

    @Override
    public void onRouteCityInfo(RouteAlongCityParam routeAlongCityParam) {
        //长途判断
        if (ConvertUtils.isEmpty(routeAlongCityParam)) return;
        routeAlongCityInfos = routeAlongCityParam.getRouteAlongCityInfos();
        boolean isLongRoute = !ConvertUtils.isEmpty(routeAlongCityInfos) && routeAlongCityInfos.get(routePackage.getSelectRouteIndex().get(MapTypeId.MAIN_SCREEN_MAIN_MAP)).getAdCityList().size() >= 2;
        mViewModel.showHideTab(isLongRoute);
        mViewModel.showHideElicCheckBox(isLongRoute);

//        if (isFirstRequest) showParkingFragment();
//        isFirstRequest = false;
    }

    private void showParkingFragment() {
        mViewModel.cancelTimer();
        Bundle bundle = new Bundle();
        GeoPoint point = new GeoPoint(routePackage.getEndPoint(MapTypeId.MAIN_SCREEN_MAIN_MAP).getRealPos().lon, routePackage.getEndPoint(MapTypeId.MAIN_SCREEN_MAIN_MAP).getRealPos().lat);
        bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, point);
        addFragment(new TerminalParkingFragment(), bundle);
    }

    @Override
    public void onRouteTrafficIncidentInfo(RouteTrafficIncidentParam routeTrafficIncidentParam) {
    }

    @Override
    public void onRouteAllRoutePoiInfo(RequestRouteResult requestRouteResult) {
        routeParams = requestRouteResult.getRouteParams();
        if (routeParams.size() >= NumberUtils.NUM_2) {
            routeParams.remove(routeParams.size() - NumberUtils.NUM_1);
            routeParams.remove(NumberUtils.NUM_0);
        }
        ThreadManager.getInstance().postUi(() -> mViewModel.setViaListUI(routeParams));
    }

    @Override
    public void onRouteFail(MapTypeId mapTypeId, String errorMsg) {
        mViewModel.hideProgressUI();
        ThreadManager.getInstance().postUi(() -> {
            ToastUtils.Companion.getInstance().showCustomToastView(errorMsg);
        });
    }

    @Override
    public void onRouteRequest() {
        searchPackage.clearLabelMark();
        clearWeatherView();
        clearRestrictionView();
        mViewModel.showProgressUI();
        mViewModel.showNomalRouteUI();
    }

    @Override
    public void onRouteSlected(MapTypeId mapTypeId, int routeIndex) {
        if (mapTypeId == MapTypeId.MAIN_SCREEN_MAIN_MAP) {
            mViewModel.updateSelectRouteUI(routeIndex);
            searchPackage.clearLabelMark();
            clearWeatherView();
            if (routeIndex >= 0 && routeIndex < routeLineInfos.size()) {
                if (!routeLineInfos.get(routeIndex).isCanBeArrive()) {
                    mViewModel.showTripDialog(ResourceUtils.Companion.getInstance().getString(R.string.route_trip_title), ResourceUtils.Companion.getInstance().getString(R.string.route_trip_elec_not_arrive));
                } else if (routeLineInfos.get(routeIndex).getRemainPercent() < 20) {
                    mViewModel.showTripDialog(ResourceUtils.Companion.getInstance().getString(R.string.route_trip_title), ResourceUtils.Companion.getInstance().getString(R.string.route_trip_elec_small));
                }
                boolean isLongRoute = !ConvertUtils.isEmpty(routeAlongCityInfos) && routeAlongCityInfos.get(routeIndex).getAdCityList().size() >= 2;
                mViewModel.showHideTab(isLongRoute);
                mViewModel.showHideElicCheckBox(isLongRoute);
            }
        }
    }

    @Override
    public void onBeforeNotifyClick(MapTypeId mapTypeId, GemBaseLayer layer, GemLayerItem pItem) {
    }

    @Override
    public void onNotifyClick(MapTypeId mapTypeId, GemBaseLayer layer, GemLayerItem pItem) {
        if (pItem.getClickBusinessType() == GemLayerClickBusinessType.BizRouteTypeWeather) {
            if (!ConvertUtils.isEmpty(routeWeatherInfos) && routeWeatherInfos.size() > pItem.getIndex()) {
                mViewModel.showWeatherDeatilsUI(routeWeatherInfos.get((int) (pItem.getIndex())));
            }
        } else if (pItem.getClickBusinessType() == GemLayerClickBusinessType.BizRouteTypeRestArea) {

        } else if (pItem.getClickBusinessType() == GemLayerClickBusinessType.BizRouteTypePath) {
            if (!ConvertUtils.isEmpty(routeLineInfos)) {
                int index = -1;
                for (int t = 0; t < routeLineInfos.size(); t++) {
                    if (pItem.getIndex() == routeLineInfos.get(t).getPathID()) {
                        index = t;
                    }
                }
                if (index != -1 && index != routePackage.getSelectRouteIndex().get(MapTypeId.MAIN_SCREEN_MAIN_MAP)) {
                    routePackage.selectRoute(MapTypeId.MAIN_SCREEN_MAIN_MAP, index);
                }
            }
        } else if (pItem.getClickBusinessType() == GemLayerClickBusinessType.BizRouteTypeTrafficEventTip) {
            PoiInfoEntity poiInfo = new PoiInfoEntity();
            poiInfo.setPid(String.valueOf(pItem.getEventId()));
            poiInfo.setPoint(new GeoPoint(pItem.getLog(), pItem.getLat(), pItem.getZ()));
            Bundle bundle = new Bundle();
            bundle.putParcelable(AutoMapConstant.TrafficEventBundleKey.BUNDLE_KEY_ENTITY, poiInfo);
            TrafficEventFragment trafficEventFragment;
            BaseFragment fragment =  StackManager.getInstance().getCurrentFragment(mViewModel.mScreenId);
            if (fragment != null && fragment instanceof TrafficEventFragment) {
                trafficEventFragment = (TrafficEventFragment) fragment;
                trafficEventFragment.setArguments(bundle);
                trafficEventFragment.onInitData();
            } else {
                trafficEventFragment = new TrafficEventFragment();
                addFragment(trafficEventFragment, bundle);
            }
        }
    }
    @Override
    public void onAfterNotifyClick(MapTypeId mapTypeId, GemBaseLayer layer, GemLayerItem pItem) {
    }

    @Override
    public void onImmersiveStatusChange(MapTypeId mapTypeId, ImersiveStatus currentImersiveStatus) {
        mViewModel.cancelTimer();
        if ((Objects.equals(NaviStatusPackage.getInstance().getCurrentNaviStatus(), NaviStatus.NaviStatusType.SELECT_ROUTE)) && currentImersiveStatus == ImersiveStatus.IMERSIVE) {
            routePackage.showPreview(MapTypeId.MAIN_SCREEN_MAIN_MAP);
        }
    }

    @Override
    public void onSearchResult(int taskId, int errorCode, String message, SearchResultEntity searchResultEntity) {
        mViewModel.hideSearchProgressUI();
        if (ConvertUtils.isEmpty(searchResultEntity)) return;
        if (localTaskId != taskId) return;
        Logger.i(TAG, GsonUtils.toJson(searchResultEntity.getPoiList()));
        if (searchResultEntity.getSearchType() == AutoMapConstant.SearchType.SEARCH_KEYWORD
                || searchResultEntity.getSearchType() == AutoMapConstant.SearchType.ALONG_WAY_SEARCH
                || searchResultEntity.getSearchType() == AutoMapConstant.SearchType.AROUND_SEARCH
                || searchResultEntity.getSearchType() == AutoMapConstant.SearchType.EN_ROUTE_KEYWORD_SEARCH) {
            if (searchResultEntity.getKeyword().equals(ResourceUtils.Companion.getInstance().getString(R.string.route_search_keyword_charge))) {
                gasChargeAlongList.clear();
                List<RouteParam> allPoiParamList = routePackage.getAllPoiParamList(MapTypeId.MAIN_SCREEN_MAIN_MAP);
                if (allPoiParamList.size() >= 2) {
                    allPoiParamList.remove(0);
                    allPoiParamList.remove(allPoiParamList.size() - 1);
                }
                gasChargeAlongList.addAll(allPoiParamList);
                mViewModel.showRouteSearchChargeListUI(searchResultEntity.getPoiList(), gasChargeAlongList ,listSearchType);
            } else if (searchResultEntity.getKeyword().equals(ResourceUtils.Companion.getInstance().getString(R.string.route_search_keyword_service))) {
                mViewModel.showRouteSearchListUI(searchResultEntity.getPoiList());
            }
        } else if (searchResultEntity.getSearchType() == AutoMapConstant.SearchType.LINE_DEEP_INFO_SEARCH || searchResultEntity.getSearchType() == AutoMapConstant.SearchType.POI_SEARCH) {
            PoiInfoEntity poiInfoEntity = searchResultEntity.getPoiList().get(0);
            if (!ConvertUtils.isEmpty(poiInfoEntity) && !ConvertUtils.isEmpty(taskMap.get(taskId))) {
                mViewModel.showRouteSearchDetailsUI(taskMap.get(taskId), poiInfoEntity, gasChargeAlongList, listSearchType);
            }
        }
    }

    @Override
    public void onSilentSearchResult(int taskId, int errorCode, String message, SearchResultEntity searchResultEntity) {
        SearchResultCallback.super.onSilentSearchResult(taskId, errorCode, message, searchResultEntity);
    }

    @Override
    public void onMarkClickCallBack(PoiInfoEntity poiInfo) {
        SearchResultCallback.super.onMarkClickCallBack(poiInfo);
    }
}

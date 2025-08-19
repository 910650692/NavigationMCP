package com.sgm.navi.service.logicpaket.hud;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.autonavi.gbl.common.path.option.PathInfo;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.StartService;
import com.sgm.navi.service.adapter.calibration.CalibrationAdapter;
import com.sgm.navi.service.adapter.calibration.CalibrationAdapterCallback;
import com.sgm.navi.service.adapter.layer.LayerAdapter;
import com.sgm.navi.service.adapter.map.IMapAdapterCallback;
import com.sgm.navi.service.adapter.map.MapAdapter;
import com.sgm.navi.service.adapter.navi.NaviAdapter;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.layer.RouteLineLayerParam;
import com.sgm.navi.service.define.layer.refix.CarModeType;
import com.sgm.navi.service.define.map.HUDMapView;
import com.sgm.navi.service.define.map.IBaseScreenMapView;
import com.sgm.navi.service.define.map.MapMode;
import com.sgm.navi.service.define.map.MapScreenShotDataInfo;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.map.ThemeType;
import com.sgm.navi.service.define.navi.NaviEtaInfo;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.logicpaket.layer.LayerPackage;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.navi.IGuidanceObserver;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.service.logicpaket.navi.OpenApiHelper;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusCallback;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.sgm.navi.service.logicpaket.position.PositionPackage;
import com.sgm.navi.service.logicpaket.route.IRouteResultObserver;
import com.sgm.navi.service.logicpaket.route.RoutePackage;

import java.util.ArrayList;
import java.util.HashMap;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/6/2
 */
public class HudPackage implements StartService.ISdkInitCallback, IMapAdapterCallback, IRouteResultObserver, IGuidanceObserver , NaviStatusCallback {
    private static final String TAG = MapDefaultFinalTag.HUD_SERVICE_TAG;
    private static final MapType HUD = MapType.HUD_MAP;
    private IBaseScreenMapView mMapSurfaceView = null;
    //车标距离底部边距
    private static final int HUD_MAP_CAR_BOTTOM = 64;
    private HashMap<String, IHudCallback> hudCallbackMap = new HashMap<>();
    private boolean hudSnowMode;

    private static final int FIVE_HUNDRED_METERS = 500;
    private static final int TWO_HUNDRED_METERS = 200;
    private static final float FOURTEEN = 14F;
    private static final float FIFTEEN = 15F;
    private static final float SIXTEEN = 16F;
    private static final int FREEWAY = 0;
    private static final int MAIN_ROAD = 7;
    private static final int CITY_SPEEDWAY = 6;

    /**
     * 导航状态
     * @param naviStatus the status of the navigation
     */
    @Override
    public void onNaviStatusChange(String naviStatus) {
        setHudZoomLevel();
    }
    /**
     * TBT面板
     */
    @Override
    public void onNaviInfo(final NaviEtaInfo naviETAInfo) {
        setHudZoomLevel();
    }

    public void setHudZoomLevel() {
        try {
            String currentNaviStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
            if (NaviStatus.NaviStatusType.NAVING.equals(currentNaviStatus)) {
                NaviEtaInfo currentNaviEtaInfo = NaviPackage.getInstance().getCurrentNaviEtaInfo();
                if (currentNaviEtaInfo == null) return;
                //int nextDist = currentNaviEtaInfo.getNextDist();
                int nextDist = currentNaviEtaInfo.getNaviInfoData().get(currentNaviEtaInfo.NaviInfoFlag).segmentRemain.dist;
                int curRoadClass = currentNaviEtaInfo.getCurRoadClass();
                Logger.d(TAG, "当前导航信息 下个路口距离:" ,nextDist, " 当前道路等级:" , curRoadClass);
                float zoomLevel;
                if (curRoadClass == CITY_SPEEDWAY || curRoadClass == MAIN_ROAD || curRoadClass == FREEWAY) {
                    zoomLevel = nextDist < FIVE_HUNDRED_METERS ? FIFTEEN : FOURTEEN;
                } else {
                    zoomLevel = nextDist < TWO_HUNDRED_METERS ? SIXTEEN : FIFTEEN;
                }
                updateZoomLevel(zoomLevel);
            } else {
                updateZoomLevel(FIFTEEN);
            }
        }catch (Exception e){
            Logger.e(TAG, "setHudZoomLevel error: " , e.getMessage());
        }
    }

    private void updateZoomLevel(float targetZoomLevel) {
        float currentZoomLevel = MapPackage.getInstance().getZoomLevel(HUD);
        Logger.d(TAG, "当前比例尺级别：" , currentZoomLevel , " 目标比例尺级别：" , targetZoomLevel);
        if (targetZoomLevel != currentZoomLevel) {
            MapPackage.getInstance().setZoomLevel(HUD, targetZoomLevel);
        }
    }


    private static final class Helper {
        private static final HudPackage hudPackage = new HudPackage();
    }

    private HudPackage() {
    }

    public void initHudService(){
        MapAdapter.getInstance().registerCallback(MapType.HUD_MAP, this);
        NaviPackage.getInstance().registerObserver(TAG, this);
        RoutePackage.getInstance().registerRouteObserver(TAG, this);
        CalibrationAdapter.getInstance().registerCallback(TAG, mCalibrationAdapterCallback);
        NaviStatusPackage.getInstance().registerObserver(TAG,this);
        hudSnowMode = CalibrationAdapter.getInstance().getHudSnowMode();
    }

    private final CalibrationAdapterCallback mCalibrationAdapterCallback = new CalibrationAdapterCallback() {
        @Override
        public void onHudSnowModeChanged(boolean snowMode) {
            Logger.i(TAG, "雪地模式 onHudSnowModeChanged" , snowMode);
            updateMapThemeType(snowMode);
        }
    };

    public void registerHudCallback(String key, IHudCallback hudCallback) {
        ConvertUtils.push(hudCallbackMap, key, hudCallback);
        Logger.d(TAG, "registerHudCallback hudCallbackMap： " + hudCallbackMap.size());
    }

    public void createHudView() {
        if (null != mMapSurfaceView) return;
        Logger.d(TAG, "init Hud Service");
        createHudView(null);
    }

    public void createHudView(IBaseScreenMapView mapSurfaceView) {
        Logger.d(TAG, "init Hud Service");
        if (null == mapSurfaceView) {
            mapSurfaceView = new HUDMapView(AppCache.getInstance().getMContext());
        }
        mMapSurfaceView = mapSurfaceView;
        boolean sdkStatus = StartService.getInstance().checkSdkIsNeedInit();
        Logger.i(TAG, "校验Sdk是否需要初始化sdkStatus：" + sdkStatus);
        if (sdkStatus) {
            StartService.getInstance().registerSdkCallback(TAG,this);
            StartService.getInstance().startInitSdk();
        }else {
            Logger.d(TAG, "HUDMapViewBindMapView......");
            MapPackage.getInstance().bindMapView(mMapSurfaceView);
        }
        Logger.d(TAG, "HUDMapView地图创建中......");
    }

    public void unInitHudService() {
        if (ConvertUtils.isEmpty(mMapSurfaceView)) return;
        RoutePackage.getInstance().unRegisterRouteObserver(TAG);
        NaviPackage.getInstance().unregisterObserver(TAG);
        NaviStatusPackage.getInstance().unregisterObserver(TAG);
        LayerPackage.getInstance().unInitLayer(MapType.HUD_MAP);
        MapAdapter.getInstance().unregisterCallback(MapType.HUD_MAP, this);
        if (StartService.getInstance().checkSdkIsAvailable()) {
            MapAdapter.getInstance().unBindMapView(mMapSurfaceView);
            MapAdapter.getInstance().destroyMapView(MapType.HUD_MAP);
        }
        Logger.d(TAG, "HUDMapView地图已销毁");
        ConvertUtils.clear(hudCallbackMap);
        mMapSurfaceView = null;
    }

    public static HudPackage getInstance() {
        return Helper.hudPackage;
    }


    @Override
    public void onSdkInitSuccess() {
        StartService.getInstance().unregisterSdkCallback(TAG, this);
        ThreadManager.getInstance().postUi(() -> MapPackage.getInstance().createMapView(MapType.HUD_MAP));
        MapPackage.getInstance().bindMapView(mMapSurfaceView);
    }

    @Override
    public void onSdkInitFail(int initSdkResult, String msg) {
        Logger.d(TAG, "引擎初始化失败重试重......");
    }


    @Override
    public void onMapLoadSuccess(MapType mapTypeId) {
        Logger.d(TAG, "onMapLoadSuccess", "mapTypeId:" + mapTypeId.name());
        if (mapTypeId == MapType.HUD_MAP){
            LayerAdapter.getInstance().initLayer(MapType.HUD_MAP);
            LayerPackage.getInstance().initGuideRouteHUDMode(MapType.HUD_MAP);
            //设置地图中心点
            setHudMapCenter(MapType.HUD_MAP);
            //回车位保存中心
            setHudMapCarPosition(MapType.HUD_MAP);
            LayerPackage.getInstance().setCarMode(MapType.HUD_MAP, CarModeType.CAR_MODE_DEFAULT);
            MapPackage.getInstance().switchHudMapMode(MapType.HUD_MAP, MapMode.UP_2D);
            LayerPackage.getInstance().setFollowMode(MapType.HUD_MAP, true);
            MapPackage.getInstance().updateUiStyle(MapType.HUD_MAP, ThemeType.NIGHT);
            LayerPackage.getInstance().closeDynamicLevel(MapType.HUD_MAP);
            long l = mMapSurfaceView.getMapViewHeight() - HUD_MAP_CAR_BOTTOM;
            MapAdapter.getInstance().setHudMapCenterInScreen(MapType.HUD_MAP,(int)mMapSurfaceView.getMapViewWidth() / 2 , (int)l);
            if (NaviStatusPackage.getInstance().getCurrentNaviStatus().equals(NaviStatus.NaviStatusType.NAVING)){
                Logger.d(TAG, "导航中显示导航路线 hud");
                hudDrawLine();
            }else {
                RoutePackage.getInstance().clearRouteLine(MapType.HUD_MAP);
            }
            Logger.d(TAG, "HUDMapView地图加载完成",MapPackage.getInstance().getZoomLevel(MapType.HUD_MAP));
            Logger.i(TAG, "雪地模式" , hudSnowMode);
            updateMapThemeType(hudSnowMode);

            if (NaviStatusPackage.getInstance().getCurrentNaviStatus().equals(NaviStatus.NaviStatusType.NAVING)){
                setHudZoomLevel();
            }
        }
    }

    @Override
    public void onMapScaleChanged(MapType mapTypeId, int currentScale) {
        //mViewModel.updateOnMapScaleChanged(currentScale);
        Logger.d(TAG, "onMapScaleChanged " , mapTypeId.name() , " " , currentScale);
    }

    @Override
    public void onSelectMainPathStatus(long pathID, int result) {
        Logger.d(TAG, "onSelectMainPathStatus");
        updateHudDrawLine(pathID);
    }

    @Override
    public void onEGLScreenshot(MapType mapTypeId, byte[] bytes, MapScreenShotDataInfo info) {
        Logger.d(TAG, "HUDMap截图数据长度：" + bytes.length, "hudCallbackMap size : " + hudCallbackMap.size());
        for (IHudCallback hudCallback : hudCallbackMap.values()) {
            hudCallback.onEGLScreenshot(mapTypeId, bytes);
        }
    }

    @Override
    public void onNaviStop() {
        Logger.d(TAG, "导航结束显示导航路线 hud");
        RoutePackage.getInstance().clearRouteLine(MapType.HUD_MAP);
    }

    @Override
    public void onNaviStart() {
        Logger.d(TAG, "导航开始显示导航路线 hud");
        hudDrawLine();
    }

    @Override
    public void onRouteDrawLine(RouteLineLayerParam routeLineLayerParam) {
        //偏航以后HUD没有重新算路
        String currentNaviStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
        Logger.d(TAG, "onRouteDrawLine", "currentNaviStatus = ", currentNaviStatus);
        if (NaviStatus.NaviStatusType.NAVING.equals(currentNaviStatus)) {
            hudDrawLine();
        }
    }
    //设置HUDMap中心点
    public void setHudMapCenter(MapType mapType){
        MapPackage.getInstance().setMapCenter(mapType, new GeoPoint(PositionPackage.getInstance().getLastCarLocation().getLongitude(),
                PositionPackage.getInstance().getLastCarLocation().getLatitude()));
    }
    //设置HUDMap车位置
    public void setHudMapCarPosition(MapType mapType) {
        LayerPackage.getInstance().setCarPosition(mapType, new GeoPoint(PositionPackage.getInstance().getLastCarLocation().getLongitude(),
                PositionPackage.getInstance().getLastCarLocation().getLatitude(), 0,
                PositionPackage.getInstance().getLastCarLocation().getCourse()));
    }

    public void hudDrawLine() {
        RoutePackage.getInstance().showOnlyOneRouteLine(MapType.HUD_MAP);
    }

    private void updateHudDrawLine(long pathID) {
        PathInfo selectPathInfo = OpenApiHelper.getPathInfo(
                MapType.MAIN_SCREEN_MAIN_MAP, pathID);
        ArrayList<PathInfo> pathInfos = new ArrayList<>();
        if (selectPathInfo != null) {
            pathInfos.add(selectPathInfo);
        }
        if (!ConvertUtils.isEmpty(pathInfos)) {
            NaviAdapter.getInstance().updatePathInfo(MapType.HUD_MAP, pathInfos,
                    0);
        }
    }

    //修改地图雪地 非雪地模式
    private void updateMapThemeType(boolean isSnowMode) {
        Logger.d(TAG, "updateMapThemeType:isSnowMode:", isSnowMode);
        ThemeType colorMode = isSnowMode ? ThemeType.DAY : ThemeType.NIGHT;
        MapAdapter.getInstance().updateUiStyle(MapType.HUD_MAP, colorMode);
    }

    @Override
    public void onChangeNaviPath(long oldPathId, long pathID) {
        Logger.i(TAG, "onChangeNaviPath oldPathId = ", oldPathId, " pathID = ", pathID);
        updateHudDrawLine(pathID);
    }

    @Override
    public void onDeletePath(ArrayList<Long> pathIDList) {
        Logger.i(TAG, "onDeletePath");
        if (!ConvertUtils.isEmpty(pathIDList)) {
            for (long pathId : pathIDList) {
                Logger.i(TAG, "onDeletePath pathId = ", pathId);
                RoutePackage.getInstance().removeRouteLineInfo(MapType.HUD_MAP, pathId);
            }
            long currentPathID = OpenApiHelper.getCurrentPathId(MapType.MAIN_SCREEN_MAIN_MAP);
            updateHudDrawLine(currentPathID);
        }
    }

}

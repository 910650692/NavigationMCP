
package com.fy.navi.service.adapter.map.bls;

import android.content.Context;
import android.content.res.Configuration;
import android.os.Looper;
import android.util.AttributeSet;
import android.view.MotionEvent;
import android.view.SurfaceHolder;

import com.android.utils.ConvertUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.autonavi.gbl.common.model.Coord2DDouble;
import com.autonavi.gbl.map.MapService;
import com.autonavi.gbl.map.MapView;
import com.autonavi.gbl.map.OperatorGesture;
import com.autonavi.gbl.map.OperatorPosture;
import com.autonavi.gbl.map.adapter.MapHelper;
import com.autonavi.gbl.map.adapter.MapSurfaceView;
import com.autonavi.gbl.map.layer.model.OpenLayerID;
import com.autonavi.gbl.map.model.DeviceAttribute;
import com.autonavi.gbl.map.model.EGLDeviceWorkMode;
import com.autonavi.gbl.map.model.GestureConfigure;
import com.autonavi.gbl.map.model.InitMapParam;
import com.autonavi.gbl.map.model.MapBusinessDataType;
import com.autonavi.gbl.map.model.MapControllerStatesType;
import com.autonavi.gbl.map.model.MapLabelItem;
import com.autonavi.gbl.map.model.MapLabelType;
import com.autonavi.gbl.map.model.MapModelDtoConstants;
import com.autonavi.gbl.map.model.MapParameter;
import com.autonavi.gbl.map.model.MapPoiCustomOperateType;
import com.autonavi.gbl.map.model.MapPositionParam;
import com.autonavi.gbl.map.model.MapRenderVendor;
import com.autonavi.gbl.map.model.MapResourceParam;
import com.autonavi.gbl.map.model.MapStyleMode;
import com.autonavi.gbl.map.model.MapStyleParam;
import com.autonavi.gbl.map.model.MapStyleTime;
import com.autonavi.gbl.map.model.MapViewParam;
import com.autonavi.gbl.map.model.MapViewPortParam;
import com.autonavi.gbl.map.model.MapViewStateType;
import com.autonavi.gbl.map.model.MapZoomScaleMode;
import com.autonavi.gbl.map.model.MapviewMode;
import com.autonavi.gbl.map.model.MapviewModeParam;
import com.autonavi.gbl.map.model.PreviewParam;
import com.autonavi.gbl.map.observer.IBLMapEngineObserver;
import com.autonavi.gbl.map.observer.IBLMapViewProxy;
import com.autonavi.gbl.map.observer.IDeviceObserver;
import com.autonavi.gbl.map.observer.IMapGestureObserver;
import com.autonavi.gbl.map.observer.IMapviewObserver;
import com.autonavi.gbl.map.observer.ITextTextureObserver;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.util.errorcode.common.Service;
import com.autonavi.gbl.util.model.SingleServiceID;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.GBLCacheFilePath;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.engine.EngineAdapter;
import com.fy.navi.service.adapter.map.IMapAdapterCallback;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.bean.PreviewParams;
import com.fy.navi.service.define.map.MapMode;
import com.fy.navi.service.define.map.MapStateStyle;
import com.fy.navi.service.define.map.MapViewParams;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.logicpaket.engine.EnginePackage;
import com.fy.navi.service.logicpaket.setting.SettingPackage;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import lombok.Getter;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/3
 */
public class MapViewImpl extends MapSurfaceView implements IMapviewObserver,
        IMapGestureObserver, IDeviceObserver, IBLMapViewProxy, IBLMapEngineObserver {

    private static final String TAG = MapDefaultFinalTag.MAP_SERVICE_TAG;

    private static float MAP_ZOOM_LEVEL_MAX = 19F;
    private static float MAP_ZOOM_LEVEL_MIN = 3F;
    private static float MAP_ZOOM_LEVEL_CHANGE_FLAG = 1F;
    private static float MAP_ZOOM_LEVEL_DEFAULT = 13F;

    private static float MAP_DEFAULT_TEXT_SIZE = 1.3F;

    @Getter
    private MapView mapview;

    @Getter
    private MapViewParams mapViewParams;

    private MapType mapType;

    // 记录当前全览状态 true：进入 false：退出
    @Getter
    private boolean isPreview = false;

    private List<IMapAdapterCallback> callbacks = new CopyOnWriteArrayList<>();

    public MapViewImpl(Context context) {
        this(context, null);
    }

    public MapViewImpl(Context context, AttributeSet attrs) {
        super(context, attrs);
    }

    public void initMapView(MapType mapType, MapViewParams mapViewParams) {
        this.mapType = mapType;
        this.mapViewParams = mapViewParams;
        createMapService();
        createMapDevice();
        createMapView();
        initOperatorPosture();
        initOperatorBusiness();
        initSkyBox();
    }

    private void createMapService() {
        MapService mapService = (MapService) ServiceMgr.getServiceMgrInstance().getBLService(SingleServiceID.MapSingleServiceID);
        setMapService(ConvertUtils.isNullRequire(mapService, "获取地图服务失败"));
        InitMapParam initMapParam = new InitMapParam();
        /*** 地图数据路径绝对地址 **/
        initMapParam.dataPath = GBLCacheFilePath.MAP_DATA_DIR;
        /*** 基本数据路径地址URL **/
        initMapParam.basePath = GBLCacheFilePath.MAP_BASE_PATH;
        /*** 配置引擎样式文件MapAssert的绝对地址 **/
        initMapParam.assetPath = GBLCacheFilePath.MAP_ASSET_DIR;
        ConvertUtils.isNullRequire(getMapService(), "初始化地图服务失败").initMap(initMapParam);
    }

    private void createMapView() {
        MapViewParam mapViewParam = new MapViewParam();
        mapViewParam.deviceId = getDefaultDevice().getDeviceId();
        Logger.i(TAG, "mapViewParam.deviceId -> " + mapViewParam.deviceId);
        mapViewParam.engineId = EnginePackage.getInstance().getEngineID(mapType);
        Logger.i(TAG, "mapViewParam.engineId -> " + mapViewParam.engineId);
        mapViewParam.x = mapViewParams.getX();
        Logger.i(TAG, "mapViewParam.x -> " + mapViewParam.x);
        mapViewParam.y = mapViewParams.getY();
        Logger.i(TAG, "mapViewParam.y -> " + mapViewParam.y);
        mapViewParam.width = mapViewParams.getWidth();
        Logger.i(TAG, "mapViewParam.width -> " + mapViewParam.width);
        mapViewParam.height = mapViewParams.getHeight();
        Logger.i(TAG, "mapViewParam.height -> " + mapViewParam.height);
        mapViewParam.screenWidth = mapViewParams.getScreenWidth();
        Logger.i(TAG, "mapViewParam.screenWidth -> " + mapViewParam.screenWidth);
        mapViewParam.screenHeight = mapViewParams.getScreenHeight();
        Logger.i(TAG, "mapViewParam.screenWidth -> " + mapViewParam.screenHeight);
        mapViewParam.cacheCountFactor = 2.0F;
        mapViewParam.zoomScaleMode = MapZoomScaleMode.PhysicalAdaptiveMode;
        mapViewParam.mapProfileName = "mapprofile_fa1"; // 星河效果指定性能模式
        setDefaultMapView(ConvertUtils.isNullRequire(getMapService().createMapView(mapViewParam,
                        this, this, null, null),
                "创建虚拟视图失败"));
    }

    private void createMapDevice() {
        ServiceMgr.getServiceMgrInstance().setUiLooper(0, Looper.getMainLooper());
        DeviceAttribute devAttribute = new DeviceAttribute();
        devAttribute.renderVendorType = MapRenderVendor.OpenGL3;
        devAttribute.uiTaskDeviceId = EngineAdapter.getInstance().mapDeviceID(mapType);
        devAttribute.deviceWorkMode = EGLDeviceWorkMode.EGLDeviceWorkMode_WithThreadWithEGLContextDrawIn;
        setDefaultDevice(ConvertUtils.isNullRequire(getMapService().createDevice(EngineAdapter.getInstance().mapDeviceID(mapType),
                        devAttribute, this),
                "创建虚拟设备失败"));
    }

    @Override
    public void setDefaultMapView(MapView mapview) {
        super.setDefaultMapView(mapview);
        this.mapview = mapview;
    }

    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        getMapview().addGestureObserver(this);
        getMapview().addMapviewObserver(this);
    }

    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        Logger.i(TAG, "MapSurfaceView:" + mapType, "已解绑窗口");
        if (ConvertUtils.isEmpty(getMapview())) return;
        getMapview().removeMapviewObserver(this);
        getMapview().removeGestureObserver(this);
    }

    public void registerCallback(IMapAdapterCallback callback) {
        if (!callbacks.contains(callback)) {
            callbacks.add(callback);
        }
    }

    public void unRegisterCallback(IMapAdapterCallback callback) {
        callbacks.remove(callback);
    }


    /**
     * 初始化底图默认比例尺设定
     */
    private void initOperatorPosture() {
        // 设置地图比例尺范围.
        getMapview().getOperatorPosture().setMinZoomLevel(MAP_ZOOM_LEVEL_MIN);
        // 设置地图比例尺范围.
        getMapview().getOperatorPosture().setMaxZoomLevel(MAP_ZOOM_LEVEL_MAX);
        getMapview().getOperatorPosture().setZoomLevel(MAP_ZOOM_LEVEL_DEFAULT, true, true);
    }

    /**
     * 初始化地图的默认配置
     */
    protected void initOperatorBusiness() {
        //设置字体缩放系数
        MapParameter mapParameter = new MapParameter();

        //设置底图默认字体大小
        getMapview().getOperatorBusiness().setMapTextScale(MAP_DEFAULT_TEXT_SIZE);

        //显示路网
        getMapview().getOperatorBusiness().showMapRoad(true);

        //显示3D建筑
        getMapview().getOperatorBusiness().showBuilding3D(true);

        //开启POI标注
        getMapview().getOperatorBusiness().setLabelVisable(true);

        //开启导航标注
        mapParameter.value1 = 1; //开启导航标注
        mapParameter.value2 = 15;//设置帧率
        mapParameter.value3 = 0;//按上层设置的帧率刷新
        mapParameter.value4 = 0;//保留
        getMapview().getOperatorBusiness().setMapBusinessDataPara(MapBusinessDataType.MAP_BUSINESSDATA_FORCE_NAVI_LABEL, mapParameter);

        //开启TMC
        getMapview().setControllerStatesOperator(MapControllerStatesType.MAP_CONTROLLER_ONOFF_TRAFFIC_STATE, 1, true);

        // 显示开放图层
        getMapview().getOperatorBusiness().showOpenLayer(OpenLayerID.OpenLayerIDRouteTraffic, true);

        //设置简易三维开
        getMapview().getOperatorBusiness().setMapViewState(MapViewStateType.MAP_VIEWSTATE_IS_SIMPLE3D_ON, true);

        //显示室内地图
        getMapview().getOperatorBusiness().setIndoorBuildingShow(true);

        //设置地形阴影图开
        getMapview().getOperatorBusiness().setMapViewState(MapViewStateType.MAP_STATE_IS_TOPOGRAPHY_SHOW, true);//开启地形阴影图
    }

    private void initSkyBox() {
        SkyBoxManager.getInstance().initSkyBox(getMapview());
    }

    @Override
    public byte[] requireMapResource(long l, MapResourceParam mapResourceParam) {
        return MapHelper.getMapAssetHelper().requireResource(getContext(), mapResourceParam);
    }

    @Override
    public void surfaceCreated(SurfaceHolder holder) {
        super.surfaceCreated(holder);
        Logger.i(TAG, "surfaceCreated", "callBacks size:" + callbacks.size());
        for (IMapAdapterCallback callback : callbacks) {
            callback.onMapInitSuccess(mapType, true);
        }
    }

    @Override
    public void onSurfaceCreated(int deviceId, int width, int height, int colorBits) {
        IDeviceObserver.super.onSurfaceCreated(deviceId, width, height, colorBits);
        for (IMapAdapterCallback callback : callbacks) {
            callback.onMapLoadSuccess(mapType);
        }
    }


    public void setZoomLevel(float level) {
        getMapview().getOperatorPosture().setZoomLevel(level, true, false);
    }

    public String getMapBound() {
        return getMapview().getOperatorPosture().getMapBound().left
                + "|" + getMapview().getOperatorPosture().getMapBound().top
                + "|" + getMapview().getOperatorPosture().getMapBound().right
                + "|" + getMapview().getOperatorPosture().getMapBound().bottom;
    }

    public void setMapCenterInScreen(int x, int y) {
        getMapview().setMapLeftTop(x, y);
    }

    public void setMapCenter(GeoPoint geoPoint) {
        getMapview().getOperatorPosture().setMapCenter(geoPoint.getLon(), geoPoint.getLat(), 0, true, true);
    }

    public boolean setTrafficStates(boolean isOpen) {
        int status = isOpen ? 1 : 0;
        boolean result = getMapview().setControllerStatesOperator(MapControllerStatesType.MAP_CONTROLLER_ONOFF_TRAFFIC_STATE, status, true);
        if (result) {
            getMapview().resetTickCount(2);
        }
        return result;
    }

    public void setCustomLabelTypeVisable(ArrayList<Integer> typeList, boolean visible) {
        int type = visible ? MapPoiCustomOperateType.CUSTOM_POI_OPERATE_ONLY_LIST_SHOW : MapPoiCustomOperateType.CUSTOM_POI_OPERATE_ONLY_LIST_HIDE;
        getMapview().getOperatorBusiness().setCustomLabelTypeVisable(typeList, type);
    }

    public void setMapViewTextSize(float f) {
        getMapview().getOperatorBusiness().setMapTextScale(f);
    }

    public float getCurrentZoomLevel() {
        return getMapview().getOperatorPosture().getZoomLevel();
    }

    public void showPreview(PreviewParams previewParams) {
        PreviewParam preview = GsonUtils.convertToT(previewParams, PreviewParam.class);
        getMapview().showPreview(preview, true, 500, -1);
    }

    public void exitPreview() {
        getMapview().exitPreview(false);
    }

    public int getCurrentScale() {
        return getMapview().getOperatorScale().getScale((int) getCurrentZoomLevel());
    }

    public MapMode getMapMode() {
        int mapMode = getMapview().getMapMode();
        switch (mapMode) {
            case MapviewMode.MapviewModeNorth:
                return MapMode.NORTH_2D;
            case MapviewMode.MapviewMode3D:
                return MapMode.UP_3D;
            case MapviewMode.MapviewModeCar:
                return MapMode.UP_2D;
        }
        return MapMode.UP_2D;
    }

    public boolean setMapMode(MapMode mapMode) {
        // 是否开启自动比例尺，如果已开启则不需要设置比例尺参数
        boolean isAutoScaleOpen = SettingPackage.getInstance().getAutoScale();
        Logger.i("setMapMode", "isAutoScaleOpen:" + isAutoScaleOpen);
        MapviewModeParam mapviewModeParam = new MapviewModeParam();
        mapviewModeParam.bChangeCenter = true;
        switch (mapMode) {
            case UP_2D:
                mapviewModeParam.mode = MapviewMode.MapviewModeCar;
                mapviewModeParam.mapZoomLevel = 14;
                SettingPackage.getInstance().setConfigKeyMapviewMode(0);
                break;
            case UP_3D:
                mapviewModeParam.mode = MapviewMode.MapviewMode3D;
                mapviewModeParam.mapZoomLevel = 17;
                SettingPackage.getInstance().setConfigKeyMapviewMode(2);
                break;
            case NORTH_2D:
                mapviewModeParam.mode = MapviewMode.MapviewModeNorth;
                mapviewModeParam.mapZoomLevel = 14;
                SettingPackage.getInstance().setConfigKeyMapviewMode(1);
                break;
        }

        int mapModel = getMapview().setMapMode(mapviewModeParam, true);
        boolean resultOk = Service.ErrorCodeOK == mapModel;
        getMapview().resetTickCount(1);
        for (IMapAdapterCallback callback : callbacks) {
            callback.onMapModeChange(mapType, mapMode);
        }
        return resultOk;
    }

    public void goToCarPosition(boolean bAnimation, boolean changeLevel) {
        MapPositionParam pos = new MapPositionParam();
        pos.lon = MapModelDtoConstants.FLOAT_INVALID_VALUE;
        pos.lat = MapModelDtoConstants.FLOAT_INVALID_VALUE;
        if (changeLevel) {
            pos.maplevel = 15.0f;
        } else {
            pos.maplevel = getCurrentZoomLevel();
        }
        getMapview().goToPosition(pos, bAnimation);
        getMapview().resetTickCount(1);
    }

    /**
     * 地图初始化时的默认风格
     */
    protected void setMapStyle(MapStateStyle mapStateStyle) {
        MapStyleParam styleParam = getMapview().getOperatorStyle().getMapStyle();
        styleParam.mode = MapStyleMode.MapModeDefault;
        int currentUiMode = getResources().getConfiguration().uiMode & Configuration.UI_MODE_NIGHT_MASK;
        styleParam.time =
                currentUiMode == Configuration.UI_MODE_NIGHT_YES ? MapStyleTime.MapTimeNight : MapStyleTime.MapTimeDay;
        switch (mapStateStyle) {
            case MAP_NAVI:
                styleParam.state = MapModelDtoConstants.MAP_MODE_SUBSTATE_NAVI_CAR;
                break;
            case MAP_CRUISE:
            case MAP_DEFAULT:
                styleParam.state = MapModelDtoConstants.MAP_MODE_SUBSTATE_NORMAL;
                break;
            case MAP_ROUTING:
                styleParam.state = MapModelDtoConstants.MAP_MODE_SUBSTATE_PREVIEW_CAR;
                break;
            case MAP_NAVI_LANE:
                styleParam.state = MapModelDtoConstants.MAP_MODE_SUBSTATE_NAVI_LANE;
                break;
        }
        styleParam.forceUpdate = true;
        getMapview().getOperatorStyle().setMapStyle(styleParam, false);
        getMapview().resetTickCount(1);
    }


    public void update3DBuildingSwitch(boolean visible) {
        if (getMapview() != null) {
            getMapview().getOperatorBusiness().setMapViewState(MapViewStateType.MAP_VIEWSTATE_IS_BUILD_MODEL_ON, visible);
            // 设置渲染持续帧数 持续帧数完成后，进入降帧处理。实际绘制帧率为：持续帧数 + 降帧帧数
            getMapview().resetTickCount(2);
            Logger.i(TAG, "update3DBuildingSwitch:" + visible, "success!");
        }
    }


    @Override
    public boolean onDoublePress(long engineId, long px, long py) {
        Logger.d(TAG, "onDoublePress px = " + px + " ,py = " + py);
        return true;
    }

    @Override
    public boolean onSinglePress(long engineId, long px, long py, boolean clickElement) {
        Logger.d(TAG, "onSinglePress px = " + px + " ,py = " + py + ",clickElement = " + clickElement);
        return true;
    }

    @Override
    public void onLongPress(long engineId, long px, long py) {
        Logger.d(TAG, "onLongPress px = " + px + " ,py = " + py);
        PoiInfoEntity poiInfo = new PoiInfoEntity();
        poiInfo.setPoiType(AutoMapConstant.SearchType.GEO_SEARCH);
        poiInfo.setPoint(getGeoPointFromScreenPosition(px, py));
        onMapClickPoi(mapType, poiInfo);
    }

    @Override
    public void onClickBlank(long engineId, float px, float py) {
        Logger.d(TAG, "onClickBlank px = " + px + " ,py = " + py);
        PoiInfoEntity poiInfo = new PoiInfoEntity();
        poiInfo.setPoiType(AutoMapConstant.SearchType.GEO_SEARCH);
        poiInfo.setPoint(getGeoPointFromScreenPosition((long) px, (long) py));
        onMapClickPoi(mapType, poiInfo);
    }

    @Override
    public void onClickLabel(long engineId, ArrayList<MapLabelItem> pLabels) {
        Logger.d(TAG, "onClickLabel MapLabelItem = " + pLabels.size());
        if (!ConvertUtils.isEmpty(pLabels)) {
            Logger.d(TAG, "onClickLabel type = " + pLabels.get(0).type);
        }
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(pLabels)) {
                MapLabelItem item = pLabels.get(0);
                PoiInfoEntity poiInfo = new PoiInfoEntity();
                Coord2DDouble coord2DDouble = OperatorPosture.mapToLonLat(item.pixel20X, item.pixel20Y);
                GeoPoint point = new GeoPoint();
                point.setLon(coord2DDouble.lon);
                point.setLat(coord2DDouble.lat);
                poiInfo.setPoint(point);
                poiInfo.setName(item.name);
                poiInfo.setPid(item.poiid);
                switch (item.type) {
                    case MapLabelType.LABEL_Type_OPENLAYER:
                        for (IMapAdapterCallback callback : callbacks) {
                            callback.onOpenLayer(mapType, poiInfo);
                        }
                        break;
                    default:
                        for (IMapAdapterCallback callback : callbacks) {
                            callback.onMapClickPoi(mapType, poiInfo);
                        }
                        break;
                }
            }
        });
    }

    @Override
    public void onMoveBegin(long engineId, long px, long py) {
        for (IMapAdapterCallback callback : callbacks) {
            callback.onMapMove(mapType, px, py, false);
        }
    }

    @Override
    public void onMoveEnd(long engineId, long px, long py) {
        for (IMapAdapterCallback callback : callbacks) {
            callback.onMapMove(mapType, px, py, true);
        }
    }

    @Override
    public boolean onTouchEvent(MotionEvent event) {
        for (IMapAdapterCallback callback : callbacks) {
            callback.onMapTouchEvent(mapType, event);
        }
        return super.onTouchEvent(event);
    }

    @Override
    public void onMapLevelChanged(long engineId, boolean bZoomIn) {
        Logger.d(TAG, "onMapLevelChanged bZoomIn = " + bZoomIn);
        ThreadManager.getInstance().postUi(() -> {
            for (IMapAdapterCallback callback : callbacks) {
                Logger.d(TAG, "onMapLevelChanged scale = " + getCurrentZoomLevel());
                Logger.d(TAG, "onMapLevelChanged scale = " + getCurrentScale());
                callback.onMapLevelChanged(mapType, getCurrentZoomLevel());
                callback.onMapScaleChanged(mapType, getCurrentScale());
            }
        });
    }

    @Override
    public void onMapCenterChanged(long engineId, double lon, double lat) {
        for (IMapAdapterCallback callback : callbacks) {
            callback.onMapCenterChanged(mapType, lon, lat);
        }
    }

    /**
     * 预览地图进入
     *
     * @param engineId engineId
     */
    @Override
    public void onMapPreviewEnter(long engineId) {
        Logger.i(TAG, "onMapPreviewEnter");
        isPreview = true;
        for (IMapAdapterCallback callback : callbacks) {
            callback.isEnterPreview(isPreview);
        }
    }

    /**
     * 预览地图退出
     *
     * @param engineId engineId
     */
    @Override
    public void onMapPreviewExit(long engineId) {
        Logger.i(TAG, "onMapPreviewExit");
        isPreview = false;
        for (IMapAdapterCallback callback : callbacks) {
            callback.isEnterPreview(isPreview);
        }
    }

    public void updateUiStyle(int uiMode) {
        final MapStyleParam styleParam = getMapview().getOperatorStyle().getMapStyle();
        final int preTime = styleParam.time;
        final boolean isNightMode = (uiMode & Configuration.UI_MODE_NIGHT_MASK) == Configuration.UI_MODE_NIGHT_YES;
        final int expectTime = isNightMode ? MapStyleTime.MapTimeNight : MapStyleTime.MapTimeDay;
        Logger.d(TAG, "isNightMode:" + isNightMode, "preTime:" + preTime, "expectTime:" + expectTime);
        if (preTime != expectTime) {
            styleParam.time = expectTime;
            getMapview().getOperatorStyle().setMapStyle(styleParam, false);
            SkyBoxManager.getInstance().updateSkyBox(getMapview(), isNightMode);
        }
    }

    private void onMapClickPoi(MapType mapType, PoiInfoEntity poiInfo) {
        for (IMapAdapterCallback callback : callbacks) {
            callback.onMapClickPoi(mapType, poiInfo);
        }
    }

    /***
     * 从屏幕坐标获取经纬度
     * @param px
     * @param py
     * @return
     */
    private GeoPoint getGeoPointFromScreenPosition(long px, long py) {
        OperatorPosture operatorPosture = getMapview().getOperatorPosture();
        Coord2DDouble coord2DDouble = operatorPosture.screenToLonLat(px, py);
        return new GeoPoint(coord2DDouble.lon, coord2DDouble.lat);
    }


    public GeoPoint mapToLonLat(double mapX, double mapY) {
        Coord2DDouble coord2DDouble = getMapview().getOperatorPosture().mapToLonLat(mapX, mapY);
        return new GeoPoint(coord2DDouble.lon, coord2DDouble.lat, 0);
    }

}
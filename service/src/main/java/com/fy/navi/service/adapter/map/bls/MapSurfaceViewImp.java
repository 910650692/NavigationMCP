
package com.fy.navi.service.adapter.map.bls;

import android.content.Context;
import android.content.res.Configuration;
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
import com.autonavi.gbl.map.model.EGLDeviceID;
import com.autonavi.gbl.map.model.EGLDeviceWorkMode;
import com.autonavi.gbl.map.model.GestureConfigure;
import com.autonavi.gbl.map.model.MapControllerStatesType;
import com.autonavi.gbl.map.model.MapLabelItem;
import com.autonavi.gbl.map.model.MapLabelType;
import com.autonavi.gbl.map.model.MapModelDtoConstants;
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
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.map.IMapAdapterCallback;
import com.fy.navi.service.adapter.map.IsEnterPreviewCallback;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.bean.PreviewParams;
import com.fy.navi.service.define.map.MapMode;
import com.fy.navi.service.define.map.MapStateStyle;
import com.fy.navi.service.define.map.MapSurfaceViewSizeParams;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.logicpaket.engine.EnginePackage;
import com.fy.navi.service.logicpaket.setting.SettingPackage;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/3
 */
public class MapSurfaceViewImp extends MapSurfaceView implements IMapviewObserver,
        IMapGestureObserver, IDeviceObserver, IBLMapViewProxy, IBLMapEngineObserver {
    private static final String TAG = MapDefaultFinalTag.MAP_SERVICE_TAG;

    private MapView mMapview;

    private MapTypeId mapTypeId;

    private List<IMapAdapterCallback> callbacks = new CopyOnWriteArrayList<>();

    // 是否进入全览状态监听 true：进入 false：退出
    private List<IsEnterPreviewCallback> isEnterPreviewCallbacks = new CopyOnWriteArrayList<>();

    // 记录当前全览状态 true：进入 false：退出
    private boolean mIsPreview = false;

    private MapSurfaceViewSizeParams mapSurfaceViewSizeParams;

    private boolean isInitSuccess = false;

    public MapSurfaceViewImp(Context context) {
        this(context, null);
    }

    public MapSurfaceViewImp(Context context, AttributeSet attrs) {
        super(context, attrs);
    }

    public void addIsEnterPreviewCallback(IsEnterPreviewCallback callback) {
        Logger.i(TAG, "addIsEnterPreviewCallback callback:" + callback);
        if (callback == null || isEnterPreviewCallbacks.contains(callback)) return;
        isEnterPreviewCallbacks.add(callback);
    }

    public void removeIsEnterPreviewCallback(IsEnterPreviewCallback callback) {
        Logger.i(TAG, "removeIsEnterPreviewCallback callback:" + callback);
        if (null == callback) return;
        isEnterPreviewCallbacks.remove(callback);
    }

    /**
     * 获取全览状态
     * @return boolean
     */
    public boolean getIsPreview() {
        return mIsPreview;
    }

    @Override
    public void setDefaultMapView(MapView mapview) {
        super.setDefaultMapView(mapview);
        mMapview = mapview;
    }

    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        mMapview.addGestureObserver(this);
        mMapview.addMapviewObserver(this);
    }

    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        Logger.i(TAG, "MapSurfaceView:" + mapTypeId, "已解绑窗口");
        if (ConvertUtils.isEmpty(mMapview)) return;
        mMapview.removeMapviewObserver(this);
        mMapview.removeGestureObserver(this);
    }

    @Override
    protected void onFinishInflate() {
        super.onFinishInflate();
    }

    public synchronized MapSurfaceViewImp createMapSurfaceViewImp(MapService mapService, MapTypeId mapTypeId, MapSurfaceViewSizeParams mapSurfaceViewSizeParam) {
        this.mapTypeId = mapTypeId;
        this.mapSurfaceViewSizeParams = mapSurfaceViewSizeParam;
        setMapService(mapService);
        DeviceAttribute devAttribute = new DeviceAttribute();
        devAttribute.renderVendorType = MapRenderVendor.OpenGL3;
        devAttribute.uiTaskDeviceId = uiTaskDeviceId(mapTypeId);
        devAttribute.deviceWorkMode = EGLDeviceWorkMode.EGLDeviceWorkMode_WithThreadWithEGLContextDrawIn;
        setDefaultDevice(ConvertUtils.isNullRequire(mapService.createDevice(mapDeviceId(mapTypeId), devAttribute, this),
                "创建虚拟设备失败"));
        MapViewParam mapViewParam = new MapViewParam();
        mapViewParam.deviceId = getDefaultDevice().getDeviceId();
        Logger.i(TAG, "mapViewParam.deviceId -> " + mapViewParam.deviceId);
        mapViewParam.engineId = EnginePackage.getInstance().getEngineID(mapTypeId);
        Logger.i(TAG, "mapViewParam.engineId -> " + mapViewParam.engineId);
        mapViewParam.x = mapSurfaceViewSizeParams.x;
        Logger.i(TAG, "mapViewParam.x -> " + mapViewParam.x);
        mapViewParam.y = mapSurfaceViewSizeParams.y;
        Logger.i(TAG, "mapViewParam.y -> " + mapViewParam.y);
        mapViewParam.width = mapSurfaceViewSizeParams.width;
        Logger.i(TAG, "mapViewParam.width -> " + mapViewParam.width);
        mapViewParam.height = mapSurfaceViewSizeParams.height;
        Logger.i(TAG, "mapViewParam.height -> " + mapViewParam.height);
        mapViewParam.screenWidth = mapSurfaceViewSizeParams.screenWidth;
        Logger.i(TAG, "mapViewParam.screenWidth -> " + mapViewParam.screenWidth);
        mapViewParam.screenHeight = mapSurfaceViewSizeParams.screenHeight;
        Logger.i(TAG, "mapViewParam.screenWidth -> " + mapViewParam.screenHeight);
        mapViewParam.cacheCountFactor = 2.0F;
        mapViewParam.zoomScaleMode = MapZoomScaleMode.PhysicalAdaptiveMode;
        mapViewParam.mapProfileName = "mapprofile_fa1"; // 星河效果指定性能模式
        setDefaultMapView(ConvertUtils.isNullRequire(mapService.createMapView(mapViewParam,
                        this, this, null, null),
                "创建虚拟视图失败"));
        return this;
    }

    public void registerCallback(IMapAdapterCallback callback) {
        if (!callbacks.contains(callback)) {
            callbacks.add(callback);
        }
    }

    public void unRegisterCallback(IMapAdapterCallback callback) {
        callbacks.remove(callback);
    }

    public void removeAllCallback() {
        if (callbacks != null) {
            callbacks.clear();
        }
    }

    private int mapDeviceId(MapTypeId mapId) {
        switch (mapId) {
            case MAIN_SCREEN_MAIN_MAP:
                return EGLDeviceID.EGLDeviceIDDefault;
            case LAUNCHER_DESK_MAP:
                return EGLDeviceID.EGLDeviceIDExternal1;
            case LAUNCHER_WIDGET_MAP:
                return EGLDeviceID.EGLDeviceIDExternal2;
        }
        return EGLDeviceID.EGLDeviceIDDefault;
    }

    private int uiTaskDeviceId(MapTypeId mapId) {
        switch (mapId) {
            case MAIN_SCREEN_MAIN_MAP:
                return EGLDeviceID.EGLDeviceIDDefault;
            case LAUNCHER_DESK_MAP:
                return EGLDeviceID.EGLDeviceIDExternal1;
            case LAUNCHER_WIDGET_MAP:
                return EGLDeviceID.EGLDeviceIDExternal2;
        }
        return EGLDeviceID.EGLDeviceIDDefault;
    }

    public void updateMapSurfaceViewSizeParams(MapSurfaceViewSizeParams mapSurfaceViewSizeParam) {
        MapViewPortParam mapViewPortParam = new MapViewPortParam(mapSurfaceViewSizeParam.x, mapSurfaceViewSizeParam.y,
                mapSurfaceViewSizeParam.width, mapSurfaceViewSizeParam.height,
                mapSurfaceViewSizeParam.screenWidth, mapSurfaceViewSizeParam.screenHeight);
        mMapview.setMapviewPort(mapViewPortParam);
    }

    public void setZoomLevel(float level) {
        mMapview.getOperatorPosture().setZoomLevel(level, true, false);
    }

    public String getMapBound() {
        return mMapview.getOperatorPosture().getMapBound().left
                + "|" + mMapview.getOperatorPosture().getMapBound().top
                + "|" + mMapview.getOperatorPosture().getMapBound().right
                + "|" + mMapview.getOperatorPosture().getMapBound().bottom;
    }

    public void setMapCenterInScreen(int x, int y) {
        mMapview.setMapLeftTop(x, y);
    }

    public void setMapCenter(GeoPoint geoPoint) {
        mMapview.getOperatorPosture().setMapCenter(geoPoint.getLon(), geoPoint.getLat(), 0, true, true);
    }

    public GeoPoint mapToLonLat(double mapX, double mapY) {
        Coord2DDouble coord2DDouble = mMapview.getOperatorPosture().mapToLonLat(mapX, mapY);
        return new GeoPoint(coord2DDouble.lon, coord2DDouble.lat, 0);
    }

    public boolean setTrafficStates(boolean isOpen) {
        int status = isOpen ? 1 : 0;
        boolean result = mMapview.setControllerStatesOperator(MapControllerStatesType.MAP_CONTROLLER_ONOFF_TRAFFIC_STATE, status, true);
        if (result) {
            mMapview.resetTickCount(2);
        }
        return result;
    }

    public void setCustomLabelTypeVisable(ArrayList<Integer> typeList, boolean visible) {
        int type = visible ? MapPoiCustomOperateType.CUSTOM_POI_OPERATE_ONLY_LIST_SHOW : MapPoiCustomOperateType.CUSTOM_POI_OPERATE_ONLY_LIST_HIDE;
        mMapview.getOperatorBusiness().setCustomLabelTypeVisable(typeList, type);
    }

    public void setMapViewTextSize(float f) {
        mMapview.getOperatorBusiness().setMapTextScale(f);
    }

    public float getCurrentZoomLevel() {
        return mMapview.getOperatorPosture().getZoomLevel();
    }

    public void showPreview(PreviewParams previewParams) {
        PreviewParam preview = GsonUtils.convertToT(previewParams, PreviewParam.class);
        mMapview.showPreview(preview, true, 500, -1);
    }

    public void exitPreview() {
        mMapview.exitPreview(false);
    }

    public int getCurrentScale() {
        return mMapview.getOperatorScale().getScale((int) getCurrentZoomLevel());
    }

    public MapMode getMapMode() {
        int mapMode = mMapview.getMapMode();
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

    public void setMapMode(MapMode mapMode) {
        // 是否开启自动比例尺，如果已开启则不需要设置比例尺参数
        boolean isAutoScaleOpen = SettingPackage.getInstance().getAutoScale();
        Logger.i("setMapMode", "isAutoScaleOpen:" + isAutoScaleOpen);
        MapviewModeParam mapviewModeParam = new MapviewModeParam();
        mapviewModeParam.bChangeCenter = true;
        switch (mapMode) {
            case UP_2D:
                mapviewModeParam.mode = MapviewMode.MapviewModeCar;
                if (!isAutoScaleOpen) {
                    mapviewModeParam.mapZoomLevel = 14;
                }
                SettingPackage.getInstance().setConfigKeyMapviewMode(0);
                break;
            case UP_3D:
                mapviewModeParam.mode = MapviewMode.MapviewMode3D;
                if (!isAutoScaleOpen) {
                    mapviewModeParam.mapZoomLevel = 17;
                }
                SettingPackage.getInstance().setConfigKeyMapviewMode(2);
                break;
            case NORTH_2D:
                mapviewModeParam.mode = MapviewMode.MapviewModeNorth;
                if (!isAutoScaleOpen) {
                    mapviewModeParam.mapZoomLevel = 14;
                }
                SettingPackage.getInstance().setConfigKeyMapviewMode(1);
                break;
        }
        mMapview.setMapMode(mapviewModeParam, true);
        mMapview.resetTickCount(1);
        for (IMapAdapterCallback callback : callbacks) {
            callback.onMapModeChange(mapTypeId, mapMode);
        }
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
        mMapview.goToPosition(pos, bAnimation);
        mMapview.resetTickCount(1);
    }

    public MapSurfaceViewSizeParams getMapSurfaceViewSizeParams() {
        return mapSurfaceViewSizeParams;
    }

    /**
     * 地图初始化时的默认风格
     */
    protected void setMapStyle(MapStateStyle mapStateStyle) {
        MapStyleParam styleParam = mMapview.getOperatorStyle().getMapStyle();
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
        mMapview.getOperatorStyle().setMapStyle(styleParam, false);
        mMapview.resetTickCount(1);
    }

    /***
     * 初始化默认手势配置
     */
    public void setGestureConfigure() {
        OperatorGesture operatorGesture = mMapview.getOperatorGesture();
        GestureConfigure configure = operatorGesture.getGestureConfigure();
        // 是否锁住手势旋转地图
        configure.bLockMapRollAngle = true;
        operatorGesture.setGestureConfigure(configure);
    }

    /**
     * 初始化底图默认比例尺设定
     */
    protected void setOperatorPosture() {
        if (ConvertUtils.isEmpty(mMapview)) return;
        // 设置地图比例尺范围.
        mMapview.getOperatorPosture().setMinZoomLevel(AutoMapConstant.MAP_ZOOM_LEVEL_MIN);
        // 设置地图比例尺范围.
        mMapview.getOperatorPosture().setMaxZoomLevel(AutoMapConstant.MAP_ZOOM_LEVEL_MAX);
        mMapview.getOperatorPosture().setZoomLevel(AutoMapConstant.MAP_ZOOM_LEVEL_DEFAULT, true, true);
    }

    /**
     * 初始化地图的默认配置
     *
     * @param screenWidth  SurfaceView Width
     * @param screenHeight SurfaceView Height
     */
    protected void setOperatorBusinessParam(long screenWidth, long screenHeight) {
        if (ConvertUtils.isEmpty(mMapview)) return;
        //设置底图默认字体大小
        mMapview.getOperatorBusiness().setMapTextScale(AutoMapConstant.MAP_DEFAULT_TEXT_SIZE);
        //地图底图POI信息icon图标显示开关
        mMapview.getOperatorBusiness().setLabelVisable(true);
        //地图底图简易3D效果
        mMapview.getOperatorBusiness().setMapViewState(MapViewStateType.MAP_VIEWSTATE_IS_SIMPLE3D_ON, true);
        //开启建筑物增长动效
        mMapview.getOperatorBusiness().setBuildingAnimateAlpha(true, false, 10);
        mMapview.getOperatorBusiness().setMapViewState(MapViewStateType.MAP_VIEWSTATE_IS_BUILD_MODEL_ON, true);
        mMapview.getOperatorBusiness().setMapViewState(MapViewStateType.MAP_VIEWSTATE_IS_ROADNAME_BOARD_ON, true);
        //地图底图路网道路名显示开关
        mMapview.getOperatorBusiness().setMapViewState(MapViewStateType.MAP_VIEWSTATE_IS_LABLE_ROADNAME_ON, true);
        //地图底图路网是否显示
        mMapview.getOperatorBusiness().showOpenLayer(OpenLayerID.OpenLayerIDRouteTraffic, true);
        mMapview.getOperatorBusiness().setMapZoomScaleAdaptive((int) screenWidth, (int) screenHeight, 0.0f);
        mMapview.getOperatorBusiness().setMapViewState(MapViewStateType.MAP_VIEWSTATE_IS_POI_ON, true);
    }

    /**
     * 地图初始化时的默认固定参数
     */
    protected void setOperatorGestureParam() {
        if (ConvertUtils.isEmpty(mMapview)) return;
        mMapview.getOperatorGesture().enableSliding(true);
        //3d移图下开启poi显示
        mMapview.getOperatorGesture().hidePoiOn3DMoving(false);
        mMapview.getOperatorGesture().hidePoiOn3DSliding(false);
    }

    /**
     * 文字字模观察者.
     *
     * @param textureObserver current mapView textureObserver
     */
    @Deprecated
    protected void setTextTextureObserver(ITextTextureObserver textureObserver) {
        if (ConvertUtils.isEmpty(mMapview)) return;
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
            callback.onMapInitSuccess(mapTypeId, true);
        }
    }

    @Override
    public void onSurfaceCreated(int deviceId, int width, int height, int colorBits) {
        IDeviceObserver.super.onSurfaceCreated(deviceId, width, height, colorBits);
        for (IMapAdapterCallback callback : callbacks) {
            callback.onMapLoadSuccess(mapTypeId);
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
        onMapClickPoi(mapTypeId, poiInfo);
    }

    @Override
    public void onClickBlank(long engineId, float px, float py) {
        Logger.d(TAG, "onClickBlank px = " + px + " ,py = " + py);
        PoiInfoEntity poiInfo = new PoiInfoEntity();
        poiInfo.setPoiType(AutoMapConstant.SearchType.GEO_SEARCH);
        poiInfo.setPoint(getGeoPointFromScreenPosition((long) px, (long) py));
        onMapClickPoi(mapTypeId, poiInfo);
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
                PoiInfoEntity poiInfo = MapResultMapper.getInstance().mapFrom(item);
                switch (item.type) {
                    case MapLabelType.LABEL_Type_OPENLAYER:
                        for (IMapAdapterCallback callback : callbacks) {
                            callback.onOpenLayer(mapTypeId, poiInfo);
                        }
                        break;
                    default:
                        for (IMapAdapterCallback callback : callbacks) {
                            callback.onMapClickPoi(mapTypeId, poiInfo);
                        }
                        break;
                }
            }
        });
    }

    @Override
    public void onMoveBegin(long engineId, long px, long py) {
        for (IMapAdapterCallback callback : callbacks) {
            callback.onMapMove(mapTypeId, px, py, false);
        }
    }

    @Override
    public void onMoveEnd(long engineId, long px, long py) {
        for (IMapAdapterCallback callback : callbacks) {
            callback.onMapMove(mapTypeId, px, py, true);
        }
    }

    @Override
    public boolean onTouchEvent(MotionEvent event) {
        for (IMapAdapterCallback callback : callbacks) {
            callback.onMapTouchEvent(mapTypeId, event);
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
                callback.onMapLevelChanged(mapTypeId, getCurrentZoomLevel());
                callback.onMapScaleChanged(mapTypeId, getCurrentScale());
            }
        });
    }

    @Override
    public void onMapCenterChanged(long engineId, double lon, double lat) {
        for (IMapAdapterCallback callback : callbacks) {
            callback.onMapCenterChanged(mapTypeId, lon, lat);
        }
    }

    /**
     * 预览地图进入
     * @param engineId engineId
     */
    @Override
    public void onMapPreviewEnter(long engineId) {
        Logger.i(TAG, "onMapPreviewEnter");
        IMapviewObserver.super.onMapPreviewEnter(engineId);
        mIsPreview = true;
        if (!ConvertUtils.isEmpty(isEnterPreviewCallbacks)) {
            for (IsEnterPreviewCallback callback : isEnterPreviewCallbacks) {
                callback.isEnterPreview(true);
            }
        }
    }

    /**
     * 预览地图退出
     * @param engineId engineId
     */
    @Override
    public void onMapPreviewExit(long engineId) {
        Logger.i(TAG, "onMapPreviewExit");
        IMapviewObserver.super.onMapPreviewExit(engineId);
        mIsPreview = false;
        if (!ConvertUtils.isEmpty(isEnterPreviewCallbacks)) {
            for (IsEnterPreviewCallback callback : isEnterPreviewCallbacks) {
                callback.isEnterPreview(false);
            }
        }
    }

    public void updateUiStyle(int uiMode) {
        final MapStyleParam styleParam = mMapview.getOperatorStyle().getMapStyle();
        final int preTime = styleParam.time;
        final boolean isNightMode = (uiMode & Configuration.UI_MODE_NIGHT_MASK) == Configuration.UI_MODE_NIGHT_YES;
        final int expectTime = isNightMode ? MapStyleTime.MapTimeNight : MapStyleTime.MapTimeDay;
        Logger.d(TAG, "isNightMode:" + isNightMode, "preTime:" + preTime, "expectTime:" + expectTime);
        if (preTime != expectTime) {
            styleParam.time = expectTime;
            mMapview.getOperatorStyle().setMapStyle(styleParam, false);
            SkyBoxManager.getInstance().updateSkyBox(mMapview, isNightMode);
        }
    }

    private void onMapClickPoi(MapTypeId mapTypeId, PoiInfoEntity poiInfo) {
        for (IMapAdapterCallback callback : callbacks) {
            callback.onMapClickPoi(mapTypeId, poiInfo);
        }
    }

    /***
     * 从屏幕坐标获取经纬度
     * @param px
     * @param py
     * @return
     */
    private GeoPoint getGeoPointFromScreenPosition(long px, long py) {
        OperatorPosture operatorPosture = mMapview.getOperatorPosture();
        Coord2DDouble coord2DDouble = operatorPosture.screenToLonLat(px, py);
        return new GeoPoint(coord2DDouble.lon, coord2DDouble.lat);
    }

    public void update3DBuildingSwitch(boolean visible) {
        if (mMapview != null) {
            mMapview.getOperatorBusiness().setMapViewState(MapViewStateType.MAP_VIEWSTATE_IS_BUILD_MODEL_ON, visible);
            // 设置渲染持续帧数 持续帧数完成后，进入降帧处理。实际绘制帧率为：持续帧数 + 降帧帧数
            mMapview.resetTickCount(2);
            Logger.i(TAG, "update3DBuildingSwitch:" + visible, "success!");
        }
    }

    protected void initSkyBox() {
        SkyBoxManager.getInstance().initSkyBox(mMapview);
    }

    public void setPitchAngle(float pitch, boolean isAnimation, boolean isSync) {
        mMapview.getOperatorPosture().setPitchAngle(pitch, isAnimation, isSync);
    }
}

package com.fy.navi.service.adapter.map.bls;

import android.content.Context;
import android.os.Looper;
import android.util.AttributeSet;
import android.view.MotionEvent;
import android.view.SurfaceHolder;

import com.android.utils.ConvertUtils;
import com.android.utils.ScreenUtils;
import com.android.utils.ThemeUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.autonavi.gbl.common.model.Coord2DDouble;
import com.autonavi.gbl.common.model.Coord3DDouble;
import com.autonavi.gbl.map.MapDevice;
import com.autonavi.gbl.map.MapService;
import com.autonavi.gbl.map.MapView;
import com.autonavi.gbl.map.OperatorPosture;
import com.autonavi.gbl.map.adapter.MapHelper;
import com.autonavi.gbl.map.adapter.MapSurfaceView;
import com.autonavi.gbl.map.layer.model.OpenLayerID;
import com.autonavi.gbl.map.model.AnmCallbackParam;
import com.autonavi.gbl.map.model.DeviceAttribute;
import com.autonavi.gbl.map.model.EGLDeviceWorkMode;
import com.autonavi.gbl.map.model.EGLSurfaceAttr;
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
import com.autonavi.gbl.map.model.PointD;
import com.autonavi.gbl.map.model.PreviewParam;
import com.autonavi.gbl.map.model.ScreenShotCallbackMethod;
import com.autonavi.gbl.map.model.ScreenShotDataInfo;
import com.autonavi.gbl.map.model.ScreenShotMode;
import com.autonavi.gbl.map.observer.IAnimationObserver;
import com.autonavi.gbl.map.observer.IBLMapBusinessDataObserver;
import com.autonavi.gbl.map.observer.IBLMapEngineObserver;
import com.autonavi.gbl.map.observer.IBLMapViewProxy;
import com.autonavi.gbl.map.observer.IDeviceObserver;
import com.autonavi.gbl.map.observer.IEGLScreenshotObserver;
import com.autonavi.gbl.map.observer.IMapGestureObserver;
import com.autonavi.gbl.map.observer.IMapviewObserver;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.util.errorcode.common.Service;
import com.autonavi.gbl.util.model.SingleServiceID;
import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.burypoint.controller.BuryPointController;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.GBLCacheFilePath;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.calibration.CalibrationAdapter;
import com.fy.navi.service.adapter.engine.EngineAdapter;
import com.fy.navi.service.adapter.map.IMapAdapterCallback;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.bean.PreviewParams;
import com.fy.navi.service.define.map.MapMode;
import com.fy.navi.service.define.map.MapStateStyle;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.map.MapViewParams;
import com.fy.navi.service.define.map.PointDataInfo;
import com.fy.navi.service.define.map.ThemeType;
import com.fy.navi.service.define.mfc.MfcController;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.logicpaket.engine.EnginePackage;
import com.fy.navi.service.logicpaket.navistatus.NaviStatusPackage;

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
        IMapGestureObserver, IDeviceObserver, IBLMapViewProxy, IBLMapEngineObserver, IAnimationObserver, IBLMapBusinessDataObserver ,  IEGLScreenshotObserver{

    private static final String TAG = MapDefaultFinalTag.MAP_SERVICE_TAG;

    private static float MAP_ZOOM_LEVEL_MAX = 20F;
    private static float MAP_ZOOM_LEVEL_MIN = 3F;
    private static float MAP_ZOOM_LEVEL_DEFAULT = 15F;
    private static float MAP_ZOOM_LEVEL_DEFAULT_3D = 17F;
    private static float MAP_ZOOM_LEVEL_DEFAULT_3D_PATCHANGLE = 40F;

    private static float MAP_DEFAULT_TEXT_SIZE = 1.3F;
    private MapDevice mMapDevice;

    @Getter
    private MapView mapview;

    @Getter
    private MapViewParams mapViewParams;

    private MapType mapType;

    // 记录当前全览状态 true：进入 false：退出
    @Getter
    private boolean isPreview = false;

    @Getter
    private int isZoomIn = -1;

    @Getter
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
        createMapDevice(mapViewParams);
        createMapView();
        initTheme();
        initOperatorPosture();
        initOperatorBusiness();
        initOperatorGesture();
        initSkyBox();
    }

    private void createMapService() {
        MapService mapService = (MapService) ServiceMgr.getServiceMgrInstance().getBLService(SingleServiceID.MapSingleServiceID);
        InitMapParam initMapParam = new InitMapParam();
        /*** 地图数据路径绝对地址 **/
        initMapParam.dataPath = GBLCacheFilePath.MAP_DATA_DIR;
        /*** 基本数据路径地址URL **/
        initMapParam.basePath = GBLCacheFilePath.MAP_BASE_PATH;
        /*** 配置引擎样式文件MapAssert的绝对地址 **/
        initMapParam.assetPath = GBLCacheFilePath.MAP_ASSET_DIR;
        if (!ConvertUtils.isNull(mapService)) {
            setMapService(mapService);
            mapService.initMap(initMapParam);
        } else {
            Logger.e(TAG, "mapService is null");
        }
    }

    private void createMapView() {
        MapViewParam mapViewParam = new MapViewParam();
        if (!ConvertUtils.isNull(getDefaultDevice())) {
            mapViewParam.deviceId = getDefaultDevice().getDeviceId();
        }
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
        if (!ConvertUtils.isNull(getMapService())) {
            setDefaultMapView(getMapService().createMapView(mapViewParam,
                    this, this, this, this));
        } else {
            Logger.e(TAG, "mapService is null");
        }
    }

    @Override
    public void onSurfaceChanged(int deviceId, int width, int height, int colorBits) {
        startScreenshot(mapType,mMapDevice,width,height,deviceId);
    }

    private void createMapDevice(MapViewParams params) {
        ServiceMgr.getServiceMgrInstance().setUiLooper(0, Looper.getMainLooper());
        DeviceAttribute devAttribute = new DeviceAttribute();
        devAttribute.renderVendorType = MapRenderVendor.OpenGL3;
        devAttribute.uiTaskDeviceId = EngineAdapter.getInstance().mapDeviceID(mapType);
        devAttribute.deviceWorkMode = EGLDeviceWorkMode.EGLDeviceWorkMode_WithThreadWithEGLContextDrawIn;
        if (!ConvertUtils.isNull(getMapService())) {
            mMapDevice = getMapService().createDevice(EngineAdapter.getInstance().mapDeviceID(mapType),
                    devAttribute, this);
            setDefaultDevice(mMapDevice);
            if (mapType == MapType.MAIN_SCREEN_MAIN_MAP) {
                EGLSurfaceAttr eglSurfaceAttr = new EGLSurfaceAttr();
                eglSurfaceAttr.nativeWindow = -1;
                eglSurfaceAttr.isOnlyCreatePBSurface = true;
                eglSurfaceAttr.width = (int) params.getScreenWidth();
                eglSurfaceAttr.height = (int) params.getScreenHeight();
                mMapDevice.attachSurfaceToDevice(eglSurfaceAttr);
            } else if (mapType == MapType.HUD_MAP){
                initScreenshotParams(mMapDevice);
            }
        } else {
            Logger.e(TAG, "mapService is null");
        }
    }

    @Override
    public void setDefaultMapView(MapView mapview) {
        super.setDefaultMapView(mapview);
        this.mapview = mapview;
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
    private void initOperatorBusiness() {
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

    /**
     * 初始化地图的默认配置
     * 增加惯性滑动方法   还有其他操作方法
     */
    private void initOperatorGesture(){
        // 开启惯性滑动
        getMapview().getOperatorGesture().enableSliding(true);
        //3D模式下 移动地图 不隐藏poi
        getMapview().getOperatorGesture().hidePoiOn3DMoving(false);
        //3D模式下 惯性滑动 不隐藏poi
        getMapview().getOperatorGesture().hidePoiOn3DSliding(false);
    }

    /***
     * 初始化默认主题
     */
    public void initTheme() {
       final MapStyleParam styleParam = getMapview().getOperatorStyle().getMapStyle();
       styleParam.time = ThemeUtils.INSTANCE.isNightModeEnabled(getContext()) ? MapStyleTime.MapTimeNight : MapStyleTime.MapTimeDay;
       getMapview().getOperatorStyle().setMapStyle(styleParam, false);
    }

    private void initSkyBox() {
        SkyBoxManager.getInstance().initSkyBox(getMapview(), ThemeUtils.INSTANCE.isNightModeEnabled(getContext()));
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        getMapview().addGestureObserver(this);
        getMapview().addMapviewObserver(this);
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        Logger.i(TAG, "MapSurfaceView:" + mapType, "已解绑窗口");
        if (ConvertUtils.isEmpty(getMapview())) return;
        getMapview().removeMapviewObserver(this);
        getMapview().removeGestureObserver(this);
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


    public void registerCallback(IMapAdapterCallback callback) {
        if (!callbacks.contains(callback)) {
            callbacks.add(callback);
        }
    }

    public void unRegisterCallback(IMapAdapterCallback callback) {
        callbacks.remove(callback);
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


    public GeoPoint getMapCenter() {
      Coord3DDouble coord3DDouble = getMapview().getOperatorPosture().getMapCenter();
      GeoPoint mapCenter = new GeoPoint();
      mapCenter.setLon(coord3DDouble.lon);
      mapCenter.setLat(coord3DDouble.lat);
      return mapCenter;
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
        if(visible){
            //getMapview().getOperatorBusiness().setCustomLabelTypeVisable(typeList, MapPoiCustomOperateType.CUSTOM_POI_OPERATE_ONLY_LIST_SHOW);
            //MapPoiCustomType
            //清除style  注意会清除addCustomStyle的style
            getMapview().getOperatorBusiness().clearCustomStyle();
        }else {
            getMapview().getOperatorBusiness().setCustomLabelTypeVisable(typeList, MapPoiCustomOperateType.CUSTOM_POI_OPERATE_ONLY_LIST_HIDE);
        }
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
        MapviewModeParam mapviewModeParam = new MapviewModeParam();
        mapviewModeParam.bChangeCenter = true;
        switch (mapMode) {
            case UP_2D:
                mapviewModeParam.mode = MapviewMode.MapviewModeCar;
                mapviewModeParam.mapZoomLevel = MAP_ZOOM_LEVEL_DEFAULT;
                break;
            case UP_3D:
                mapviewModeParam.mode = MapviewMode.MapviewMode3D;
                mapviewModeParam.mapZoomLevel = MAP_ZOOM_LEVEL_DEFAULT_3D;
                mapviewModeParam.pitchAngle = MAP_ZOOM_LEVEL_DEFAULT_3D_PATCHANGLE;
                break;
            case NORTH_2D:
                mapviewModeParam.mode = MapviewMode.MapviewModeNorth;
                mapviewModeParam.mapZoomLevel = MAP_ZOOM_LEVEL_DEFAULT;
                break;
        }
        int mapModel = getMapview().setMapMode(mapviewModeParam, true);
        boolean resultOk = Service.ErrorCodeOK == mapModel;
        getMapview().resetTickCount(1);
        return resultOk;
    }

    @Override
    public void onMapModeChanged(long engineId, int mapMode) {
        for (IMapAdapterCallback callback : callbacks) {
            switch (mapMode) {
                case MapviewMode.MapviewMode3D -> {
                    callback.onMapModeChange(mapType, MapMode.UP_3D);
                }
                case MapviewMode.MapviewModeCar -> {
                    callback.onMapModeChange(mapType, MapMode.UP_2D);
                }
                default -> {
                    callback.onMapModeChange(mapType, MapMode.NORTH_2D);
                }
            }
        }
    }

    public void goToCarPosition(boolean bAnimation, boolean changeLevel) {
        MapPositionParam pos = new MapPositionParam();
        pos.lon = MapModelDtoConstants.FLOAT_INVALID_VALUE;
        pos.lat = MapModelDtoConstants.FLOAT_INVALID_VALUE;
        if (changeLevel) {
            int mapMode = getMapview().getMapMode();
            if(MapviewMode.MapviewMode3D == mapMode){
                pos.maplevel = 17.0f;
            }else {
                pos.maplevel = 15.0f;
            }
        } else {
            pos.maplevel = getCurrentZoomLevel();
        }
        Logger.e(TAG,"goToCarPosition " + changeLevel);
        getMapview().goToPosition(pos, bAnimation);
        getMapview().resetTickCount(1);
    }


    /**
     * mfc 控制地图上下左右移动
     * mfcController 方向
     * moveDistance  距离
     */
    public void mfcMoveMap(MfcController mfcController,int moveDistance){
        Coord3DDouble mapCenter = getMapview().getOperatorPosture().getMapCenter();
        PointD pointD = getMapview().getOperatorPosture().lonLatToScreen(mapCenter.lon,mapCenter.lat,0);
        double centerX = pointD.x;
        double centerY = pointD.y;
        if(mfcController == MfcController.LEFT){
            centerX = centerX - moveDistance;
        }else if(mfcController == MfcController.RIGHT){
            centerX = centerX + moveDistance;
        }else if(mfcController == MfcController.UP){
            centerY = centerY - moveDistance;
        }else if(mfcController == MfcController.DOWN){
            centerY = centerY + moveDistance;
        }

        Coord2DDouble coord2DDouble = getMapview().getOperatorPosture().
                screenToLonLat(centerX,centerY);
        Coord3DDouble moveCenter = new Coord3DDouble();
        moveCenter.lon = coord2DDouble.lon;
        moveCenter.lat = coord2DDouble.lat;
        getMapview().getOperatorPosture().setMapCenter(moveCenter);
    }

    /**
     * 地图初始化时的默认风格
     */
    protected void setMapStyle(MapStateStyle mapStateStyle) {
        MapStyleParam styleParam = getMapview().getOperatorStyle().getMapStyle();
        styleParam.mode = MapStyleMode.MapModeDefault;
        boolean isNightMode = ThemeUtils.INSTANCE.isNightModeEnabled(getContext());
        Logger.i(TAG, "setMapStyle:" + isNightMode);
        styleParam.time = isNightMode ? MapStyleTime.MapTimeNight : MapStyleTime.MapTimeDay;
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
        Logger.e(TAG,"setMapStyle " );
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
    public void onMove(long engineId, long px, long py) {
        IMapGestureObserver.super.onMove(engineId, px, py);

        if (NaviStatusPackage.getInstance().isGuidanceActive()) {
            sendBuryPointForZoomWithTwoFingers(true);
        }
    }

    @Override
    public void onScaleRotateEnd(long engineId, long focusX, long focusY) {
        IMapGestureObserver.super.onScaleRotateEnd(engineId, focusX, focusY);
        Logger.d(TAG, "onScaleRotateEnd focusX = " + focusX + " ,focusY = " + focusY);
        if (NaviStatusPackage.getInstance().isGuidanceActive() && isZoomIn != -1) {
            sendBuryPointForZoomWithTwoFingers(false);
        }
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
        isZoomIn = bZoomIn ? 0 : 1;
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

    public void updateUiStyle(ThemeType uiMode) {
        final MapStyleParam styleParam = getMapview().getOperatorStyle().getMapStyle();
        final int preTime = styleParam.time;

        final int expectTime = uiMode == ThemeType.NIGHT ? MapStyleTime.MapTimeNight : MapStyleTime.MapTimeDay;
        Logger.d(TAG, "preTime:" + preTime, "expectTime:" + expectTime);
        if (preTime != expectTime) {
            styleParam.time = expectTime;
            getMapview().getOperatorStyle().setMapStyle(styleParam, false);
            SkyBoxManager.getInstance().updateSkyBox(getMapview(), uiMode == ThemeType.NIGHT);
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

    public PointDataInfo lonLatToScreen(double lon, double lat, double z){
        PointD pointD = getMapview().getOperatorPosture().lonLatToScreen(lon,lat,z);
        if(ConvertUtils.isEmpty(pointD)){
            return null;
        }
        PointDataInfo pointDataInfo = new PointDataInfo();
        pointDataInfo.setMleftscreen(pointD.x);
        pointDataInfo.setMtopscreen(pointD.y);
        return pointDataInfo;
    }

    @HookMethod
    private void sendBuryPointForZoomWithTwoFingers(boolean isOneFinger) {
        StringBuilder eventName = new StringBuilder();
        if (!isOneFinger) {
            eventName.append(switch (isZoomIn) {
                case 0 -> BuryConstant.EventName.AMAP_NAVI_MAP_MANUAL_EMPLIFY_SLIDE;
                case 1 -> BuryConstant.EventName.AMAP_NAVI_MAP_MANUAL_REDUCE_SLIDE;
                default -> BuryConstant.EventName.AMAP_UNKNOWN;
            });
            isZoomIn = -1;
        } else {
            eventName.append(BuryConstant.EventName.AMAP_NAVI_MAP_MANUAL_SLIDE);
        }
        BuryPointController.getInstance().setEventName(eventName.toString());
    }

    @Override
    public void processMapAnimationFinished(long l, AnmCallbackParam anmCallbackParam) {

    }

    @Override
    public boolean onBusinessDataObserver(int i, long l, long l1) {
        return false;
    }

    public void changeMapViewParams(MapViewParams mapViewParams) {
        this.mapViewParams = mapViewParams;
        long x = mapViewParams.getX(); //左上角 左偏置
        long y = mapViewParams.getY(); //左上角 顶偏置
        long width = mapViewParams.getWidth(); //地图宽
        long height = mapViewParams.getHeight(); //地图高
        long screenWidth = mapViewParams.getScreenWidth(); //屏幕宽
        long screenHeight = mapViewParams.getScreenHeight(); //屏幕高
        MapViewPortParam mapViewPortParam = new MapViewPortParam(x, y, width, height, screenWidth, screenHeight);
        getMapview().setMapviewPort(mapViewPortParam);
    }
    /**
     * 开始截图
     * @param mapDevice 地图设备对象
     */
    public void initScreenshotParams(MapDevice mapDevice) {
        Logger.d(TAG, "takeMapScreenshot");
        if (mapDevice == null) {
            return;
        }
        Logger.d(TAG, "attachSurfaceToDevice"+mapType);
        // 设置 EGL Surface 属性
        EGLSurfaceAttr eglSurfaceAttr = new EGLSurfaceAttr();
        //eglSurfaceAttr.nativeWindow = -1;
        eglSurfaceAttr.isOnlyCreatePBSurface = true;
        eglSurfaceAttr.width = 328;
        eglSurfaceAttr.height = 172;
        mapDevice.attachSurfaceToDevice(eglSurfaceAttr);
    }
    public void startScreenshot(MapType mapType, MapDevice mapDevice,int width, int height,int deviceId){
        if (mapType == MapType.MAIN_SCREEN_MAIN_MAP){
            // 这里裁剪的区域不要随意改变，现在是整个屏幕，如果需要某一部分，需要在回调后自己再次裁剪
            mapDevice.setScreenshotRect(0, 0, width, height);
        }else if (mapType == MapType.HUD_MAP && deviceId == EngineAdapter.getInstance().mapDeviceID(MapType.HUD_MAP)){
            mapDevice.setScreenshotRect(0, 0, 328, 172);
        }
        mapDevice.setScreenshotCallBackMethod(ScreenShotCallbackMethod.ScreenShotCallbackMethodBuffer);
        //截图模式
        mapDevice.setScreenshotMode(ScreenShotMode.ScreenShotModeBackGround,this);
        Logger.d(TAG, "takeMapScreenshot mode");

    }

    @Override
    public void onEGLScreenshot(int i, byte[] pBitmapBuffer, ScreenShotDataInfo screenShotDataInfo, int i1, long l) {
        Logger.d(TAG, "onEGLScreenshot");
        if (pBitmapBuffer == null) {
            return;
        }
        Logger.d(TAG, "onEGLScreenshot"+pBitmapBuffer.length);
        for (IMapAdapterCallback callback : callbacks) {
            Logger.d(TAG, "onEGLScreenshot"+mapType);
            callback.onEGLScreenshot(mapType,pBitmapBuffer);
        }
    }
}
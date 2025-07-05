
package com.sgm.navi.service.adapter.map.bls;


import android.content.Context;
import android.graphics.Rect;
import android.util.AttributeSet;
import android.view.MotionEvent;
import android.view.SurfaceHolder;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ThemeUtils;
import com.android.utils.file.FileUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.LooperType;
import com.android.utils.thread.ThreadManager;
import com.autonavi.gbl.common.model.Coord2DDouble;
import com.autonavi.gbl.common.model.Coord3DDouble;
import com.autonavi.gbl.common.model.RectDouble;
import com.autonavi.gbl.map.MapDevice;
import com.autonavi.gbl.map.MapService;
import com.autonavi.gbl.map.MapView;
import com.autonavi.gbl.map.OperatorPosture;
import com.autonavi.gbl.map.adapter.MapHelper;
import com.autonavi.gbl.map.adapter.MapSurfaceView;
import com.autonavi.gbl.map.layer.model.OpenLayerID;
import com.autonavi.gbl.map.model.BusinessDeviceThreadMode;
import com.autonavi.gbl.map.model.DeviceAttribute;
import com.autonavi.gbl.map.model.EGLDeviceWorkMode;
import com.autonavi.gbl.map.model.EGLSurfaceAttr;
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
import com.autonavi.gbl.map.model.MapSkyboxParam;
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
import com.autonavi.gbl.map.observer.IBLMapEngineObserver;
import com.autonavi.gbl.map.observer.IBLMapViewProxy;
import com.autonavi.gbl.map.observer.IDeviceObserver;
import com.autonavi.gbl.map.observer.IEGLScreenshotObserver;
import com.autonavi.gbl.map.observer.IMapGestureObserver;
import com.autonavi.gbl.map.observer.IMapviewObserver;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.util.errorcode.common.Service;
import com.autonavi.gbl.util.model.BinaryStream;
import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.burypoint.controller.BuryPointController;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.GBLCacheFilePath;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.R;
import com.sgm.navi.service.adapter.engine.EngineAdapter;
import com.sgm.navi.service.adapter.map.IMapAdapterCallback;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.bean.PreviewParams;
import com.sgm.navi.service.define.map.MapMode;
import com.sgm.navi.service.define.map.MapScreenShotDataInfo;
import com.sgm.navi.service.define.map.MapStateStyle;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.map.MapViewParams;
import com.sgm.navi.service.define.map.PointDataInfo;
import com.sgm.navi.service.define.map.ThemeType;
import com.sgm.navi.service.define.mfc.MfcController;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusPackage;

import java.util.ArrayList;

import lombok.Getter;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/3
 */
public class MapViewImpl extends MapSurfaceView implements IMapviewObserver, IMapGestureObserver, IDeviceObserver,
        IBLMapViewProxy, IEGLScreenshotObserver, IBLMapEngineObserver {

    private static final String TAG = MapDefaultFinalTag.MAP_SERVICE_TAG;

    @Getter
    private MapView mapview;

    @Getter
    private MapViewParams mapViewParams;

    private MapType mapType;

    // 记录当前全览状态 true：进入 false：退出
    private boolean isPreview = false;

    private boolean isSplit = false;

    private int isZoomIn = -1;

    private IMapAdapterCallback callback;
    private Rect crossImgScreenshotRect;

    public MapViewImpl(Context context, MapType mapType, MapService mapService) {
        this(context, null);
        this.mapType = mapType;
        this.mapViewParams = new MapViewParams();
        setMapService(mapService);
        setDefaultDevice(createMapDevice());
        setDefaultMapView(createMapView());
        initOperatorPosture();
        initOperatorBusiness();
        initTheme(ThemeUtils.INSTANCE.isNightModeEnabled(getContext()));
        initSkyBox(ThemeUtils.INSTANCE.isNightModeEnabled(getContext()));
        Logger.d(MapDefaultFinalTag.INIT_SERVICE_TAG, mapType, " 初始化 底图成功");
    }

    public MapViewImpl(Context context, AttributeSet attrs) {
        super(context, attrs);
    }


    private MapDevice createMapDevice() {
        int deviceId = EngineAdapter.getInstance().mapDeviceID(mapType);
        DeviceAttribute devAttribute = new DeviceAttribute();
        devAttribute.renderVendorType = MapRenderVendor.OpenGL3;//星河效果
        devAttribute.uiTaskDeviceId = deviceId;
        devAttribute.deviceWorkMode = EGLDeviceWorkMode.EGLDeviceWorkMode_WithThreadWithEGLContextDrawIn;
        devAttribute.businessDeviceThreadMode = BusinessDeviceThreadMode.WithInnerWorkThread;
        devAttribute.isNeedAntialias = true;
        devAttribute.samples = 2;//TODO 性能优化配置 全屏抗锯齿倍数
        ServiceMgr.getServiceMgrInstance().setUiLooper(deviceId, ThreadManager.getInstance().getLooper(LooperType.valueOf(mapType.name())));
        Logger.d(TAG, mapType, "createMapDevice deviceId=", deviceId);
        return ConvertUtils.isNullRequire(getMapService().createDevice(deviceId, devAttribute, this),
                "获取对应的 MapService 失败 : " + mapType);
    }

    private MapView createMapView() {
        MapViewParam mapViewParam = new MapViewParam();
        mapViewParam.x = mapViewParams.getX();
        mapViewParam.y = mapViewParams.getY();
        mapViewParam.width = mapViewParams.getWidth();
        mapViewParam.height = mapViewParams.getHeight();
        mapViewParam.screenWidth = mapViewParams.getScreenWidth();
        mapViewParam.screenHeight = mapViewParams.getScreenHeight();
        mapViewParam.engineId = EngineAdapter.getInstance().engineID(mapType);
        mapViewParam.deviceId = getDefaultDevice().getDeviceId();
        mapViewParam.cacheCountFactor = 2;//TODO 性能优化配置
        //引擎配置文件 星河效果高(mapprofile_fa1 3D超远视角+天空盒)/中(mapprofile_fa2 3D常规视角+天空盒)/低(mapprofile_fa3 3D无天空盒子视角 )
        mapViewParam.mapProfileName = "mapprofile_fa2";//TODO 性能优化配置
        mapViewParam.zoomScaleMode = MapZoomScaleMode.PhysicalAdaptiveMode;//TODO 性能优化配置
        mapViewParam.asyncTaskThreadCount = 4;//TODO 性能优化配置
        return ConvertUtils.isNullRequire(getMapService().createMapView(mapViewParam, this, this, null, null),
                "获取对应的 MapView 失败 : " + mapType);
    }

    /**
     * 初始化底图默认比例尺设定
     */
    private void initOperatorPosture() {
        // 设置地图比例尺范围.
        getMapview().getOperatorPosture().setMinZoomLevel(AutoMapConstant.MAP_ZOOM_LEVEL_MIN);
        // 设置地图比例尺范围.
        getMapview().getOperatorPosture().setMaxZoomLevel(AutoMapConstant.MAP_ZOOM_LEVEL_MAX);
        getMapview().getOperatorPosture().setZoomLevel(mapType == MapType.HUD_MAP ? AutoMapConstant.MAP_ZOOM_LEVEL_DEFAULT_HUD : AutoMapConstant.MAP_ZOOM_LEVEL_DEFAULT, true, true);
        // 开启惯性滑动
        getMapview().getOperatorGesture().enableSliding(true);
    }

    /**
     * 初始化地图的默认配置
     */
    private void initOperatorBusiness() {
        //设置字体缩放系数
        MapParameter mapParameter = new MapParameter();
        //开启导航标注
        mapParameter.value1 = 1; //开启导航标注
        mapParameter.value2 = 15;//设置帧率
        mapParameter.value3 = 0;//按上层设置的帧率刷新
        mapParameter.value4 = 0;//保留
        getMapview().getOperatorBusiness().setMapBusinessDataPara(MapBusinessDataType.MAP_BUSINESSDATA_FORCE_NAVI_LABEL, mapParameter);
        //设置底图默认字体大小
        getMapview().getOperatorBusiness().setMapTextScale(AutoMapConstant.MAP_DEFAULT_TEXT_SIZE);
        //开启POI标注
        getMapview().getOperatorBusiness().setLabelVisable(true);
        //开启TMC
        getMapview().setControllerStatesOperator(MapControllerStatesType.MAP_CONTROLLER_ONOFF_TRAFFIC_STATE, mapType == MapType.HUD_MAP ? 0 : 1, true);
        //显示路网
        getMapview().getOperatorBusiness().setMapViewState(MapViewStateType.MAP_VIEWSTATE_IS_LABLE_ROADNAME_ON, true);
        //显示3D建筑
        getMapview().getOperatorBusiness().setMapViewState(MapViewStateType.MAP_VIEWSTATE_IS_BUILD_MODEL_ON, true);
        //设置简易三维开
        getMapview().getOperatorBusiness().setMapViewState(MapViewStateType.MAP_VIEWSTATE_IS_SIMPLE3D_ON, true);
        // 显示开放图层
        getMapview().getOperatorBusiness().showOpenLayer(OpenLayerID.OpenLayerIDRouteTraffic, true);
        //设置地形阴影图开
        getMapview().getOperatorBusiness().setMapViewState(MapViewStateType.MAP_STATE_IS_TOPOGRAPHY_SHOW, true);//开启地形阴影图
    }


    /***
     * 初始化默认主题
     */
    private void initTheme(boolean isNight) {
        MapStyleParam styleParam = getMapview().getOperatorStyle().getMapStyle();
        styleParam.time = isNight ? MapStyleTime.MapTimeNight : MapStyleTime.MapTimeDay;
        getMapview().getOperatorStyle().setMapStyle(styleParam, false);
    }


    private void initSkyBox(boolean isNight) {
        String skyBoxPath = isNight ? GBLCacheFilePath.MAP_SKY_BOX_ASSET_DIR + "skybox_night.data" : GBLCacheFilePath.MAP_SKY_BOX_ASSET_DIR + "skybox_day.data";
        byte[] buffer = FileUtils.getInstance().getAssetFileContent(skyBoxPath);
        if (!ConvertUtils.isNull(buffer)) {
            //设置天空盒子
            MapSkyboxParam mapSkyboxParam = new MapSkyboxParam();
            mapSkyboxParam.isOn = true; // 是否开启skybox，默认为true，非必须设置
            mapSkyboxParam.is3DRes = false; // 是否使用3D资源，*.data资源为2D天空卷轴，传false
            mapSkyboxParam.DataBuff = new BinaryStream(buffer);
            getMapview().getOperatorBusiness().setMapSkyboxParam(mapSkyboxParam);
            getMapview().resetTickCount(1);
            Logger.d(TAG, mapType, " initSkyBox");
        }
    }

    public void setCallbacks(IMapAdapterCallback callback) {
        this.callback = callback;
    }

    public void unRegisterCallback() {
        this.callback = null;
    }

    public void setZoomLevel(float level) {
        getMapview().getOperatorPosture().setZoomLevel(level, true, false);
    }

    public String getMapBound() {
        RectDouble rectDouble = getMapview().getOperatorPosture().getMapBound();
        return new StringBuffer(String.valueOf(rectDouble.left))
                .append("|")
                .append(String.valueOf(rectDouble.top))
                .append("|")
                .append(String.valueOf(rectDouble.right))
                .append("|")
                .append(String.valueOf(rectDouble.bottom)).toString();
    }

    public void setMapCenterInScreen(int x, int y) {
        getMapview().setMapLeftTop(x, y);
        Logger.d(TAG, mapType, " setMapCenterInScreen");
    }

    public void setHudMapCenterInScreen(int x, int y) {
        getMapview().setMapLeftTop(x, y);
        Logger.d(TAG, mapType, " setHudMapCenterInScreen");
    }

    public void setMapCenter(GeoPoint geoPoint) {
        getMapview().getOperatorPosture().setMapCenter(geoPoint.getLon(), geoPoint.getLat(), 0, true, true);
        Logger.d(TAG, mapType, " setMapCenter");
    }

    public GeoPoint getMapCenter() {
        Coord3DDouble coord3DDouble = getMapview().getOperatorPosture().getMapCenter();
        GeoPoint mapCenter = new GeoPoint();
        mapCenter.setLon(coord3DDouble.lon);
        mapCenter.setLat(coord3DDouble.lat);
        return mapCenter;
    }

    public void resetTickCount(int tickCount) {
        getMapview().resetTickCount(tickCount);
    }

    public boolean setTrafficStates(boolean isOpen) {
        int status = isOpen ? 1 : 0;
        boolean result = getMapview().setControllerStatesOperator(MapControllerStatesType.MAP_CONTROLLER_ONOFF_TRAFFIC_STATE, status, true);
        resetTickCount(1);
        return result;
    }

    public void setCustomLabelTypeVisable(ArrayList<Integer> typeList, boolean visible) {
        if (visible) {
            //清除style  注意会清除addCustomStyle的style
            getMapview().getOperatorBusiness().clearCustomStyle();
        } else {
            getMapview().getOperatorBusiness().setCustomLabelTypeVisable(typeList, MapPoiCustomOperateType.CUSTOM_POI_OPERATE_ONLY_LIST_HIDE);
        }
    }

    public void setMapViewTextSize(float f) {
        getMapview().getOperatorBusiness().setMapTextScale(f);
    }

    public void isSplitScreen(boolean isSplit) {
        this.isSplit = isSplit;
    }

    /**
     * 设置是否锁住手势双指捏合缩放
     * @param isLock
     */
    public void setLockMapPinchZoom(boolean isLock){
        if(getMapview() != null){
            getMapview().getOperatorGesture().lockMapPinchZoom(isLock);
        }
    }

    /**
     * 设置是否锁住手势移动
     * @param isLock
     */
    public void setLockMapMove(boolean isLock){
        if(getMapview() != null){
            getMapview().getOperatorGesture().lockMapMove(isLock);
        }
    }

    public float getCurrentZoomLevel() {
        return getMapview().getOperatorPosture().getZoomLevel();
    }

    public void showPreview(PreviewParams previewParams) {
        PreviewParam preview = GsonUtils.convertToT(previewParams, PreviewParam.class);
        getMapview().showPreview(preview, true, 500, -1);
        if(Logger.openLog) Logger.d(TAG, "showPreview");
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
                mapviewModeParam.mapZoomLevel = AutoMapConstant.MAP_ZOOM_LEVEL_DEFAULT;
                break;
            case UP_3D:
                mapviewModeParam.mode = MapviewMode.MapviewMode3D;
                mapviewModeParam.mapZoomLevel = AutoMapConstant.MAP_ZOOM_LEVEL_DEFAULT_3D;
                mapviewModeParam.pitchAngle = AutoMapConstant.MAP_ZOOM_LEVEL_DEFAULT_3D_PATCHANGLE;
                break;
            case NORTH_2D:
                mapviewModeParam.mode = MapviewMode.MapviewModeNorth;
                mapviewModeParam.mapZoomLevel = AutoMapConstant.MAP_ZOOM_LEVEL_DEFAULT;
                break;
        }
        int resultOk = getMapview().setMapMode(mapviewModeParam, true);
        getMapview().resetTickCount(1);
        Logger.d(TAG, mapType, "setMapMode ", mapMode);
        return resultOk == Service.ErrorCodeOK;
    }

    public void goToCarPosition(boolean bAnimation, boolean changeLevel) {
        MapPositionParam pos = new MapPositionParam();
        pos.lon = MapModelDtoConstants.FLOAT_INVALID_VALUE;
        pos.lat = MapModelDtoConstants.FLOAT_INVALID_VALUE;
        if (changeLevel) {
            int mapMode = getMapview().getMapMode();
            if (MapviewMode.MapviewMode3D == mapMode) {
                pos.maplevel = 17.0f;
            } else {
                pos.maplevel = 15.0f;
            }
        } else {
            pos.maplevel = getCurrentZoomLevel();
        }
        Logger.d(TAG, mapType, "goToCarPosition ", changeLevel);
        getMapview().goToPosition(pos, bAnimation);
    }

    /**
     * mfc 控制地图上下左右移动
     * mfcController 方向
     * moveDistance  距离
     */
    public void mfcMoveMap(MfcController mfcController, int moveDistance) {
        Coord3DDouble mapCenter = getMapview().getOperatorPosture().getMapCenter();
        PointD pointD = getMapview().getOperatorPosture().lonLatToScreen(mapCenter.lon, mapCenter.lat, 0);
        double centerX = pointD.x;
        double centerY = pointD.y;
        if (mfcController == MfcController.LEFT) {
            centerX = centerX - moveDistance;
        } else if (mfcController == MfcController.RIGHT) {
            centerX = centerX + moveDistance;
        } else if (mfcController == MfcController.UP) {
            centerY = centerY - moveDistance;
        } else if (mfcController == MfcController.DOWN) {
            centerY = centerY + moveDistance;
        }

        Coord2DDouble coord2DDouble = getMapview().getOperatorPosture().
                screenToLonLat(centerX, centerY);
        Coord3DDouble moveCenter = new Coord3DDouble();
        moveCenter.lon = coord2DDouble.lon;
        moveCenter.lat = coord2DDouble.lat;
        getMapview().getOperatorPosture().setMapCenter(moveCenter);
    }

    /**
     * 地图初始化时的默认风格
     */
    public void setMapStyle(MapStateStyle mapStateStyle) {
        MapStyleParam styleParam = getMapview().getOperatorStyle().getMapStyle();
        styleParam.mode = MapStyleMode.MapModeDefault;
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
        Logger.d(TAG, mapType, "setMapStyle ", mapStateStyle);
        getMapview().getOperatorStyle().setMapStyle(styleParam, false);
    }

    public void update3DBuildingSwitch(boolean visible) {
        getMapview().getOperatorBusiness().setMapViewState(MapViewStateType.MAP_VIEWSTATE_IS_BUILD_MODEL_ON, visible);
        Logger.d(TAG, mapType, "update3DBuildingSwitch ", visible);
    }

    public void updateUiStyle(ThemeType uiMode) {
        final MapStyleParam styleParam = getMapview().getOperatorStyle().getMapStyle();
        final int preTime = styleParam.time;
        final int expectTime = uiMode == ThemeType.NIGHT ? MapStyleTime.MapTimeNight : MapStyleTime.MapTimeDay;
        Logger.d(TAG, "preTime:", preTime, "expectTime:", expectTime);
        if (preTime != expectTime) {
            styleParam.time = expectTime;
            initTheme(uiMode == ThemeType.NIGHT);
            initSkyBox(uiMode == ThemeType.NIGHT);
        }
    }

    /**
     * 设置poi是否可点击
     */
    public void setMapLabelClickable(boolean enable) {
        getMapview().getOperatorGesture().setMapLabelClickable(enable);
    }

    private void onMapClickPoi(MapType mapType, PoiInfoEntity poiInfo) {
        if (callback != null) {
            ThreadManager.getInstance().postUi(new Runnable() {
                @Override
                public void run() {
                    callback.onMapClickPoi(mapType, poiInfo);
                }
            });
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

    public PointDataInfo lonLatToScreen(double lon, double lat, double z) {
        PointD pointD = getMapview().getOperatorPosture().lonLatToScreen(lon, lat, z);
        if (ConvertUtils.isEmpty(pointD)) {
            return null;
        }
        PointDataInfo pointDataInfo = new PointDataInfo();
        pointDataInfo.setMleftscreen(pointD.x);
        pointDataInfo.setMtopscreen(pointD.y);
        return pointDataInfo;
    }

    public void changeMapViewParams(MapViewParams mapViewParams) {
        this.mapViewParams = mapViewParams;
        updateMapPortParams(
                mapViewParams.getX(),
                mapViewParams.getY(),
                mapViewParams.getWidth(),
                mapViewParams.getHeight(),
                mapViewParams.getScreenWidth(),
                mapViewParams.getScreenHeight()
        );
    }

    private void updateMapPortParams(long x, long y, long width, long height, long screenWidth, long screenHeight) {
        Logger.d(TAG, mapType, "updateMapPortParams");
        MapViewPortParam mapViewPortParam = new MapViewPortParam(x, y, width, height, screenWidth, screenHeight);
        getMapview().setMapviewPort(mapViewPortParam);
    }

    public void destroyMapView() {
        getDefaultDevice().detachSurfaceFromDevice();
        getMapview().removeGestureObserver(this);
        getMapview().removeMapviewObserver(this);
        getMapService().destroyMapView(getMapview());
        setDefaultMapView(null);
        getDefaultDevice().attachSurfaceToDevice(null);
        getDefaultDevice().removeDeviceObserver(this);
        getMapService().destroyDevice(getDefaultDevice());
    }

    /**
     * 初始化截图参数
     */
    private void initScreenshotParams() {
        Logger.d(TAG, mapType, "需要支持截屏，开启Device渲染线程 截图尺寸 width =", +mapViewParams.getWidth(), "height =", mapViewParams.getHeight());
        EGLSurfaceAttr eglSurfaceAttr = new EGLSurfaceAttr();
        eglSurfaceAttr.nativeWindow = -1;
        eglSurfaceAttr.isOnlyCreatePBSurface = true;
        eglSurfaceAttr.width = (int) mapViewParams.getWidth();
        eglSurfaceAttr.height = (int) mapViewParams.getHeight();
        eglSurfaceAttr.initColor = ResourceUtils.Companion.getInstance().getColor(R.color.route_charge_param_color);
        getDefaultDevice().attachSurfaceToDevice(eglSurfaceAttr);
    }

    private void startScreenshot() {
        if (ConvertUtils.isNull(crossImgScreenshotRect)) {
            Logger.e(TAG, "updateScreenshotRect failed , rect is null!");
            return;
        }
        final int mapHeight = mapViewParams == null ? 0 : (int) mapViewParams.getScreenHeight();
        final int left = crossImgScreenshotRect.left;
        final int top = mapHeight - crossImgScreenshotRect.bottom;
        getDefaultDevice().setScreenshotRect(left, top, crossImgScreenshotRect.width(), crossImgScreenshotRect.height());
        getDefaultDevice().setScreenshotMode(ScreenShotMode.ScreenShotModeBackGround, this);
        getDefaultDevice().setScreenshotCallBackMethod(ScreenShotCallbackMethod.ScreenShotCallbackMethodBuffer);
        Logger.d(TAG, "startScreenshot-success!");
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
        Logger.d(TAG, "sendBuryPointForZoomWithTwoFingers");
        BuryPointController.getInstance().setEventName(eventName.toString());
    }

    @Override
    public void setDefaultMapView(MapView mapview) {
        this.mapview = mapview;
        super.setDefaultMapView(mapview);
    }

    @Override
    protected void onAttachedToWindow() {
        super.onAttachedToWindow();
        getMapview().addGestureObserver(this);
        getMapview().addMapviewObserver(this);
        Logger.d(TAG, mapType, " onAttachedToWindow");
    }

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        if (ConvertUtils.isEmpty(getMapview())) return;
        getMapview().removeMapviewObserver(this);
        getMapview().removeGestureObserver(this);
        Logger.d(TAG, mapType, " onDetachedFromWindow");
    }

    @Override
    public void surfaceCreated(SurfaceHolder holder) {
        super.surfaceCreated(holder);
        if (callback != null) {
            Logger.d(TAG, mapType, " onMapLoadSuccess");
            callback.onMapLoadSuccess(mapType);
        }
    }

    @Override
    public void onSurfaceChanged(int deviceId, int width, int height, int colorBits) {
        boolean openScreen = mapViewParams.isOpenScreen();
        Logger.d(TAG, mapType, "onSurfaceChanged", "openScreen", openScreen);
        if (openScreen) startScreenshot();
    }

    @Override
    public byte[] requireMapResource(long l, MapResourceParam mapResourceParam) {
        return MapHelper.getMapAssetHelper().requireResource(getContext(), mapResourceParam);
    }

    @Override
    public void onMapModeChanged(long engineId, int mapMode) {
        if (callback != null) {
            ThreadManager.getInstance().postUi(new Runnable() {
                @Override
                public void run() {
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
            });
        }
        Logger.d(TAG, mapType, " onMapModeChanged");
    }

    @Override
    public boolean onDoublePress(long engineId, long px, long py) {
        return true;
    }

    @Override
    public boolean onSinglePress(long engineId, long px, long py, boolean clickElement) {
        return true;
    }

    @Override
    public void onLongPress(long engineId, long px, long py) {
        if(isSplit) return;
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
        if (ConvertUtils.isEmpty(pLabels)) {
            return;
        }
        if (callback != null) {
            MapLabelItem item = pLabels.get(0);
            PoiInfoEntity poiInfo = new PoiInfoEntity();
            Coord2DDouble coord2DDouble = OperatorPosture.mapToLonLat(item.pixel20X, item.pixel20Y);
            GeoPoint point = new GeoPoint();
            point.setLon(coord2DDouble.lon);
            point.setLat(coord2DDouble.lat);
            poiInfo.setPoint(point);
            poiInfo.setName(item.name);
            poiInfo.setPid(item.poiid);
            ThreadManager.getInstance().postUi(new Runnable() {
                @Override
                public void run() {
                    switch (item.type) {
                        case MapLabelType.LABEL_Type_OPENLAYER:
                            callback.onOpenLayer(mapType, poiInfo);
                            break;
                        default:
                            callback.onMapClickPoi(mapType, poiInfo);
                            break;
                    }
                }
            });
        }
    }

    @Override
    public void onMove(long engineId, long px, long py) {
        if (NaviStatusPackage.getInstance().isGuidanceActive()) {
            sendBuryPointForZoomWithTwoFingers(true);
        }
    }

    @Override
    public void onScaleRotateEnd(long engineId, long focusX, long focusY) {
        if (NaviStatusPackage.getInstance().isGuidanceActive() && isZoomIn != -1) {
            sendBuryPointForZoomWithTwoFingers(false);
        }
    }

    @Override
    public void onMoveBegin(long engineId, long px, long py) {
        if (callback != null) {
            ThreadManager.getInstance().postUi(new Runnable() {
                @Override
                public void run() {
                    callback.onMapMove(mapType, px, py, false);
                }
            });
        }
    }

    @Override
    public void onMoveEnd(long engineId, long px, long py) {
        if (callback != null) {
            ThreadManager.getInstance().postUi(new Runnable() {
                @Override
                public void run() {
                    callback.onMapMove(mapType, px, py, true);
                }
            });
        }
    }


    @Override
    public boolean onTouchEvent(MotionEvent event) {
        Logger.d(TAG, mapType, " onTouchEvent:", event.getAction());
        if (callback != null) {
            ThreadManager.getInstance().postUi(new Runnable() {
                @Override
                public void run() {
                    callback.onMapTouchEvent(mapType, event);
                }
            });
        }
        return super.onTouchEvent(event);
    }

    @Override
    public void onMapLevelChanged(long engineId, boolean bZoomIn) {
        isZoomIn = bZoomIn ? 0 : 1;
        if (callback != null) {
            ThreadManager.getInstance().postUi(() -> {
                float level = getCurrentZoomLevel();
                if (level < 12F) {
                    getMapview().getOperatorBusiness().showOpenLayer(OpenLayerID.OpenLayerIDRouteTraffic, false);
                } else {
                    getMapview().getOperatorBusiness().showOpenLayer(OpenLayerID.OpenLayerIDRouteTraffic, true);
                }
                callback.onMapLevelChanged(mapType, level);
                callback.onMapScaleChanged(mapType, getCurrentScale());
            });
        }
    }

    @Override
    public void onMapCenterChanged(long engineId, double lon, double lat) {
        if (callback != null) {
            ThreadManager.getInstance().postUi(new Runnable() {
                @Override
                public void run() {
                    callback.onMapCenterChanged(mapType, lon, lat);
                }
            });
        }
    }

    /**
     * 预览地图进入
     *
     * @param engineId engineId
     */
    @Override
    public void onMapPreviewEnter(long engineId) {
        isPreview = true;
        if (callback != null) {
            ThreadManager.getInstance().postUi(new Runnable() {
                @Override
                public void run() {
                    callback.isEnterPreview(mapType, isPreview);
                }
            });
        }
    }

    /**
     * 预览地图退出
     *
     * @param engineId engineId
     */
    @Override
    public void onMapPreviewExit(long engineId) {
        isPreview = false;
        if (callback != null) {
            ThreadManager.getInstance().postUi(new Runnable() {
                @Override
                public void run() {
                    callback.isEnterPreview(mapType, isPreview);
                }
            });
        }
    }

    @Override
    public void onEGLScreenshot(int i, byte[] pBitmapBuffer, ScreenShotDataInfo screenShotDataInfo, int i1, long l) {
        if (callback != null) {
            callback.onEGLScreenshot(mapType, pBitmapBuffer, GsonUtils.convertToT(screenShotDataInfo, MapScreenShotDataInfo.class));
        }
    }

    public void openOrCloseScreenshot(boolean isOpen) {
        Logger.i(TAG, mapType, "openOrCloseScreenshot", isOpen);
        if (isOpen) {
            getDefaultDevice().setScreenshotMode(ScreenShotMode.ScreenShotModeBackGround, this);
            getDefaultDevice().setScreenshotCallBackMethod(ScreenShotCallbackMethod.ScreenShotCallbackMethodBuffer);
        } else {
            getDefaultDevice().setScreenshotMode(ScreenShotMode.ScreenShotModeNull, null);
        }
    }

    public void updateScreenshotRect(Rect rect) {
        crossImgScreenshotRect = rect;
        if (mapViewParams.isOpenScreen()) {
            initScreenshotParams();
            startScreenshot();
        }
    }
}
package com.fy.navi.service.define.hud;
import android.content.Context;
import com.android.utils.log.Logger;
import com.fy.navi.service.StartService;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.map.MapVisibleAreaType;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.map.IMapPackageCallback;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.navi.IGuidanceObserver;
import com.fy.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.fy.navi.service.logicpaket.position.PositionPackage;
import com.fy.navi.service.logicpaket.setting.SettingPackage;
/**
 * HUD地图管理类：负责地图的加载与销毁
 */
public class HudMapManager implements StartService.ISdkInitCallback, IMapPackageCallback, SettingPackage.SettingChangeCallback, IGuidanceObserver {

    private static final String TAG = "HudMapManager";

    // 单例实例
    private static final HudMapManager instance = new HudMapManager();

    private final MapPackage mMapPackage;
    private final LayerPackage mLayerPackage;
    private final PositionPackage mPositionPackage;
    private NaviStatusPackage mNaviStatusPackage;
    private SettingPackage mSettingPackage;
    private boolean isMapInitialized = false;
    protected boolean isShowCrossImage;//是否正在显示路口大图
    private HudMapManager() {
        Logger.d(TAG, "onCreate");
        // 私有构造函数
        StartService.getInstance().registerSdkCallback(this);
        mMapPackage = MapPackage.getInstance();
        mLayerPackage = LayerPackage.getInstance();
        mPositionPackage = PositionPackage.getInstance();
        mNaviStatusPackage = NaviStatusPackage.getInstance();
        mSettingPackage = SettingPackage.getInstance();
    }

    public static HudMapManager getInstance() {
        return instance;
    }

    @Override
    public void onSdkInitSuccess() {
        Logger.d(TAG, "Sdk init success");
    }

    @Override
    public void onSdkInitFail(int initSdkResult, String msg) {
        Logger.d(TAG, "Sdk init fail");
    }

    @Override
    public void onMapLoadSuccess(MapType mapTypeId) {
        Logger.d(TAG, "onMapLoadSuccess:" + mapTypeId.name());
        if (mapTypeId == MapType.CLUSTER_MAP) {
            mMapPackage.setZoomLevel(mapTypeId, 13);
        }
        if (mapTypeId == MapType.CLUSTER_MAP) {
            LayerPackage.getInstance().setCarPosition(MapType.CLUSTER_MAP, new GeoPoint(PositionPackage.getInstance().getLastCarLocation().getLongitude(),
                    PositionPackage.getInstance().getLastCarLocation().getLatitude(), 0,
                    PositionPackage.getInstance().getLastCarLocation().getCourse()));
            //设置默认的车标
            mLayerPackage.setDefaultCarMode(mapTypeId);
            //地图中心
            mMapPackage.setMapCenter(mapTypeId, new GeoPoint(mPositionPackage.getLastCarLocation().getLongitude(),
                    mPositionPackage.getLastCarLocation().getLatitude()));
            //回车位
            mMapPackage.goToCarPosition(mapTypeId);
            mLayerPackage.openDynamicLevel(mapTypeId, true);
            //设置跟随模式
            mLayerPackage.setFollowMode(mapTypeId, true);
            //设置地图中线点在屏幕中的位置
            mMapPackage.changMapCenterInScreen(mapTypeId, MapVisibleAreaType.MAIN_AREA_CAR);
            mSettingPackage.setSettingChangeCallback(mapTypeId.name(), this);
        }
    }

    @Override
    public void onSettingChanged(String key, String value) {
        SettingPackage.SettingChangeCallback.super.onSettingChanged(key, value);
        Logger.d(TAG, "onSettingChanged:" + key + ":" + value);
        mLayerPackage.setCarMode(MapType.HUD_MAP, mLayerPackage.getCarModeType(MapType.MAIN_SCREEN_MAIN_MAP));
    }

    public void loadMap(Context context) {
        if (isMapInitialized) return;
        Logger.d("HudMapManager", "HUD地图加载中...");
        isMapInitialized = true;
    }

    public void destroyMap() {
        if (!isMapInitialized) return;
        Logger.d("HudMapManager", "HUD地图已销毁");
        isMapInitialized = false;
    }
    @Override
    public void onCrossImageInfo(boolean isShowImage, CrossImageEntity naviImageInfo) {
        Logger.d(TAG, "onCrossImageInfo isShowImage:" + isShowImage + " naviImageInfo:" + naviImageInfo);
//        int type = 0;
//        byte[] bytes = null;
//        byte[] bytes1 = null;
//        if (naviImageInfo != null) {
//            type = naviImageInfo.getType();
//            bytes = naviImageInfo.getDataBuf();
//            bytes1 = naviImageInfo.getArrowDataBuf();
//        }
//        Rect viewRect = new Rect();
//        viewRect.left = 0;      // 左边距为 0（贴左边）
//        viewRect.top = 0;       // 上边距为 0（贴顶部）
//        viewRect.right = 500;   // 宽度为 300
//        viewRect.bottom = 320;  // 高度为 300

    }

//    @Override
//    public void onHideCrossImage(int type) {
////        Logger.d(TAG, "onHideCrossImage: type = " + type);
////        isShowCrossImage = false;
////        mCrossLinkId = -1;
////        mCrossSegmentId = -1;
////        NaviConfigManager.getInstance().getDrivingLayer(SurfaceViewID.transformDisplayId2SurfaceId(mFragment.getDisplayId())).hideCross(type);
////        //隐藏路口大图卡片
////        if (mMvpView != null) {
////            mMvpView.hideCross();
////        }
////        if(mIsEnterLane) {
////            //进入车道的状态无需处理退出放大路口图逻辑
////            return;
////        }
////        if (mSettingMsp!=null){
////            mCurrentScale = mSettingMsp.getIntValue(MapSharePreference.SharePreferenceKeyEnum.autoScale, 1);
////        }
////
////        if (mCurrentScale == 1) {
////            NaviConfigManager.getInstance().getDrivingLayer(SurfaceViewID.transformDisplayId2SurfaceId(mFragment.getDisplayId())).openDynamicLevel(true, DynamicLevelType.DynamicLevelGuide);//开启动态比例尺
////        }
////        //地图投影中心回到地图中心点
////        DisplayMetrics displayMetrics = ScreenUtils.getDisplayMetrics(SurfaceViewID.transformDisplayId2SurfaceId(mFragment.getDisplayId()));
////        IModuleMapService mapService = (IModuleMapService) ((AutoContext) SdkApplicationUtils.getApplication()).getAutoService(AutoContext.MODULE_SERVICE_BASEMAP);
////        Logger.d(TAG, "onHideCrossImage setMapLeftTop widthPixels:{?}, heightPixels:{?}", displayMetrics.widthPixels, displayMetrics.heightPixels);
////        mapService.setMapLeftTop(SurfaceViewID.transformDisplayId2SurfaceId(mFragment.getDisplayId()), displayMetrics.widthPixels / 2, displayMetrics.heightPixels * 4 / 5);
////        SdkAdapterManager.getInstance().sendCrossMessage(203,0);
//    }

//    public static boolean showCrossImage(@SurfaceViewID.SurfaceViewID1 int surfaceViewID, CrossImageInfo info) {
//        boolean ret = false;
//        Logger.d(TAG, "showCrossImage, surfaceViewID={?}, info={?}", surfaceViewID, info);
//
//        DrivingLayer drivingLayer = SDKManager.getInstance().getLayerController().getDrivingLayer(surfaceViewID);
//        if (drivingLayer == null) {
//            Logger.d(TAG, "showCrossImage, drivingLayer==null");
//            return ret;
//        }
//        if (info.type == CrossType.CrossTypeVector || info.type == CrossType.CrossType3D) {
//            //矢量图或者三维图
//            ret = drivingLayer.updateCross(info.dataBuf, info.type);
//        } else if (info.type == CrossType.CrossTypeGrid) {
//            //栅格图
//            int RES_ID = 888888888;
//            LayerTexture arrowImge = new LayerTexture();
//            LayerTexture roadImage = new LayerTexture();
//            arrowImge.dataBuff = new BinaryStream(info.arrowDataBuf);
//            //栅格图箭头png
//            arrowImge.iconType = LayerIconType.LayerIconTypePNG;
//            arrowImge.resID = RES_ID;
//            arrowImge.isGenMipmaps = false;
//            arrowImge.isPreMulAlpha = true;
//            arrowImge.isRepeat = false;
//            arrowImge.anchorType = LayerIconAnchor.LayerIconAnchorLeftTop;
//
//            roadImage.dataBuff = new BinaryStream(info.dataBuf);
//            //栅格图背景图jpg
//            roadImage.iconType = LayerIconType.LayerIconTypeJPG;
//            roadImage.resID = RES_ID;
//            roadImage.isGenMipmaps = false;
//            roadImage.isPreMulAlpha = true;
//            roadImage.isRepeat = false;
//            roadImage.anchorType = LayerIconAnchor.LayerIconAnchorLeftTop;
//
//            ret = drivingLayer.setRasterImageData(arrowImge, roadImage);
//        }
//
//        // 需要主动触发显示（2D矢量路口大图第一次显示的时候默认隐藏）
//        if (info.type != CrossType.CrossType3D) {
//            drivingLayer.setRoadCrossVisible(info.type, ret);
//        }
//        LayerAdapter.getInstance().updateRoadCrossRect(MapType.HUD_MAP,viewRect);
//        return ret;
//    }
}

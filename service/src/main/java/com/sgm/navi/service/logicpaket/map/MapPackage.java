package com.sgm.navi.service.logicpaket.map;

import android.os.Bundle;
import android.view.MotionEvent;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.layer.ILayerAdapterCallBack;
import com.sgm.navi.service.adapter.layer.LayerAdapter;
import com.sgm.navi.service.adapter.map.IMapAdapterCallback;
import com.sgm.navi.service.adapter.map.MapAdapter;
import com.sgm.navi.service.adapter.position.PositionAdapter;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.bean.MapLabelItemBean;
import com.sgm.navi.service.define.bean.PreviewParams;
import com.sgm.navi.service.define.map.IBaseScreenMapView;
import com.sgm.navi.service.define.map.MapMode;
import com.sgm.navi.service.define.map.MapStateStyle;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.map.ThemeType;
import com.sgm.navi.service.define.mfc.MfcController;
import com.sgm.navi.service.define.position.LocInfoBean;
import com.sgm.navi.service.define.search.PoiInfoEntity;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.function.Consumer;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public class MapPackage implements IMapAdapterCallback, ILayerAdapterCallBack {
    private static final String TAG = MapDefaultFinalTag.MAP_SERVICE_TAG;
    private MapAdapter mMapAdapter;
    private final HashMap<MapType, List<IMapPackageCallback>> callbacks = new HashMap<>();

    private static final class Helper {
        private static final MapPackage ep = new MapPackage();
    }

    public static MapPackage getInstance() {
        return Helper.ep;
    }

    private MapPackage() {
        mMapAdapter = MapAdapter.getInstance();
    }

    public void initMapService() {
        mMapAdapter.initMapService();
        mMapAdapter.registerCallback(MapType.MAIN_SCREEN_MAIN_MAP, this);
    }

    public boolean createMapView(MapType mapTypeId) {

        return mMapAdapter.createMapView(mapTypeId);
    }

    public void bindMapView(IBaseScreenMapView mapSurfaceView) {
        mMapAdapter.bindMapView(mapSurfaceView);
        mMapAdapter.registerCallback(mapSurfaceView.provideMapTypeId(), this);
    }

    public void unBindMapView(IBaseScreenMapView mapSurfaceView) {
        mMapAdapter.unBindMapView(mapSurfaceView);
        mMapAdapter.unregisterCallback(mapSurfaceView.provideMapTypeId(), this);
    }

    public void destroyMapView(MapType mapType) {
        mMapAdapter.destroyMapView(mapType);
    }

    public void unInitMapService() {
        mMapAdapter.unInitMapService();
    }

    public void registerCallback(MapType mapTypeId, IMapPackageCallback callback) {
        if (!callbacks.containsKey(mapTypeId)) {
            callbacks.put(mapTypeId, new ArrayList<>());
        }
        if (!callbacks.get(mapTypeId).contains(callback)) {
            callbacks.get(mapTypeId).add(callback);
        }
    }

    public void unRegisterCallback(MapType mapTypeId, IMapPackageCallback callback) {
        if (callbacks.get(mapTypeId) != null) {
            callbacks.get(mapTypeId).remove(callback);
        }
    }

    public void reduceLevel(MapType mapTypeId) {
        mMapAdapter.reduceLevel(mapTypeId);
    }

    public void setZoomLevel(MapType mapTypeId, float level) {
        mMapAdapter.setZoomLevel(mapTypeId, level);
    }

    public float getZoomLevel(MapType mapTypeId) {
        return mMapAdapter.getZoomLevel(mapTypeId);
    }

    /* 判断当前mapview是否存在 */
    public boolean isMapViewExist(MapType mapTypeId) {
        return mMapAdapter.isMapViewExist(mapTypeId);
    }

    public void amplifyLevel(MapType mapTypeId) {
        mMapAdapter.amplifyLevel(mapTypeId);
    }

    public void setMapCenterInScreen(MapType mapTypeId, int x, int y) {
        mMapAdapter.setMapCenterInScreen(mapTypeId, x, y);
    }

    public void setMapCenter(MapType mapTypeId, GeoPoint geoPoint) {
        mMapAdapter.setMapCenter(mapTypeId, geoPoint);
    }

    public GeoPoint getMapCenter(MapType mapTypeId) {
        return mMapAdapter.getMapCenter(mapTypeId);
    }

    public boolean isCarLocation(MapType mapTypeId, double maxDistance) {
        GeoPoint mapCenter = getMapCenter(mapTypeId);
        LocInfoBean locInfoBean = PositionAdapter.getInstance().getLastCarLocation();
        if (!ConvertUtils.isEmpty(mapCenter) && !ConvertUtils.isEmpty(locInfoBean)) {
            GeoPoint carLocInfo = new GeoPoint(locInfoBean.getLongitude(), locInfoBean.getLatitude());
            double distance = mMapAdapter.calcStraightDistance(mapCenter, carLocInfo);
            BigDecimal num1 = new BigDecimal(distance);
            BigDecimal num2 = new BigDecimal(maxDistance);
            //判断num1是否大于num2
            int result = num1.compareTo(num2);
            return result > 0;
        }
        return false;
    }

    public void setMapViewTextSize(MapType mapTypeId, float f) {
        mMapAdapter.setMapViewTextSize(mapTypeId, f);
    }

    public boolean switchMapMode(MapType mapTypeId) {
        return mMapAdapter.switchMapMode(mapTypeId);
    }

    public MapMode getCurrentMapMode(MapType mapTypeId) {
        return mMapAdapter.getCurrentMapMode(mapTypeId);
    }

    public boolean switchMapMode(MapType mapTypeId, MapMode mapMode, boolean isSave) {
        return mMapAdapter.setMapMode(mapTypeId, mapMode, isSave);
    }

    public void setMapStateStyle(MapType mapTypeId, MapStateStyle mapStateStyle) {
        mMapAdapter.setMapStateStyle(mapTypeId, mapStateStyle);
    }

    public void goToCarPosition(MapType mapTypeId) {
        mMapAdapter.goToCarPosition(mapTypeId, false, true);
    }

    public void goToCarPosition(MapType mapTypeId, boolean bAnimation, boolean changeLevel) {
        mMapAdapter.goToCarPosition(mapTypeId, bAnimation, changeLevel);
    }

    public void mfcMoveMap(MapType mapTypeId, MfcController mfcController, int moveDistance) {
        mMapAdapter.mfcMoveMap(mapTypeId, mfcController, moveDistance);
        //TODO ？
        if (callbacks.containsKey(mapTypeId) && callbacks.get(mapTypeId) != null) {
            List<IMapPackageCallback> callbacks = this.callbacks.get(mapTypeId);
            if (ConvertUtils.isEmpty(callbacks)) return;
            for (IMapPackageCallback callback : callbacks) {
                callback.onMapTouchEvent(mapTypeId, null);
            }
        }
    }

    public void setMapLabelClickable(MapType mapTypeId, boolean enable) {
        mMapAdapter.setMapLabelClickable(mapTypeId, enable);
    }

    //TODO ？
    public void resetTickCount(MapType mapTypeId, int tickCount) {
        mMapAdapter.resetTickCount(mapTypeId, tickCount);
    }

    public String getMapBound(MapType mapTypeId) {
        return mMapAdapter.getMapBound(mapTypeId);
    }

    public void showPreview(MapType mapTypeId, PreviewParams previewParams) {
        mMapAdapter.showPreview(mapTypeId, previewParams);
        LayerAdapter.getInstance().setPreviewMode(mapTypeId, true);
    }

    /**
     * 显示预览
     *
     * @param mapTypeId
     * @param isRouteLine  不知道传什么，可以传默认值true
     * @param screenLeft
     * @param screenTop
     * @param screenRight
     * @param screenBottom
     * @param mapBound
     */
    public void showPreview(MapType mapTypeId, boolean isRouteLine, int screenLeft, int screenTop, int screenRight, int screenBottom, PreviewParams.RectDouble mapBound) {
        PreviewParams previewParams = new PreviewParams();
        previewParams.setMapBound(mapBound);
        previewParams.setbUseRect(true);
        previewParams.setRouteLine(isRouteLine);
        previewParams.setScreenLeft(screenLeft);
        previewParams.setScreenTop(screenTop);
        previewParams.setScreenRight(screenRight);
        previewParams.setScreenBottom(screenBottom);
        showPreview(mapTypeId, previewParams);
        if (mapBound != null) {
            Logger.i(TAG, "mapBound: " + mapBound.toString());
        } else {
            Logger.i(TAG, "mapBound: null");
        }
    }

    public void showPreview(MapType mapTypeId, boolean isRouteLine, int screenLeft, int screenTop, int screenRight, int screenBottom, List<PreviewParams.PointD> points) {
        PreviewParams previewParams = new PreviewParams();
        previewParams.setPoints(points);
        previewParams.setbUseRect(false);
        previewParams.setRouteLine(isRouteLine);
        previewParams.setScreenLeft(screenLeft);
        previewParams.setScreenTop(screenTop);
        previewParams.setScreenRight(screenRight);
        previewParams.setScreenBottom(screenBottom);
        showPreview(mapTypeId, previewParams);
        Logger.d(TAG, "points size " + points.size());
    }

    public void showPreview(MapType mapTypeId, boolean isRouteLine, PreviewParams.RectDouble mapBound) {
        PreviewParams previewParams = new PreviewParams();
        previewParams.setMapBound(mapBound);
        previewParams.setbUseRect(true);
        previewParams.setRouteLine(isRouteLine);
        showPreview(mapTypeId, previewParams);
        if (mapBound != null) {
            Logger.i(TAG, "mapBound: " + mapBound.toString());
        } else {
            Logger.i(TAG, "mapBound: null");
        }
    }

    public void exitPreview(MapType mapTypeId) {
        LayerAdapter.getInstance().setPreviewMode(mapTypeId, false);
        mMapAdapter.exitPreview(mapTypeId);
    }


    @Override
    public void onMapCenterChanged(MapType mapTypeId, double lon, double lat) {
        if (callbacks.containsKey(mapTypeId)) {
            callbacks.get(mapTypeId).forEach(new Consumer<IMapPackageCallback>() {
                @Override
                public void accept(IMapPackageCallback callback) {
                    callback.onMapCenterChanged(mapTypeId, lon, lat);
                }
            });
        }
    }

    @Override
    public void onMapLevelChanged(MapType mapTypeId, float mapLevel) {
        if (callbacks.containsKey(mapTypeId)) {
            callbacks.get(mapTypeId).forEach(new Consumer<IMapPackageCallback>() {
                @Override
                public void accept(IMapPackageCallback callback) {
                    callback.onMapLevelChanged(mapTypeId, mapLevel);
                }
            });
        }
    }

    @Override
    public void onMapClickBlank(MapType mapTypeId, float px, float py) {
        if (callbacks.containsKey(mapTypeId)) {
            callbacks.get(mapTypeId).forEach(new Consumer<IMapPackageCallback>() {
                @Override
                public void accept(IMapPackageCallback callback) {
                    callback.onMapClickBlank(mapTypeId, px, py);
                }
            });
        }
    }

    @Override
    public void onMapClickLabel(MapType mapTypeId, ArrayList<MapLabelItemBean> pLabels) {
        if (callbacks.containsKey(mapTypeId)) {
            callbacks.get(mapTypeId).forEach(new Consumer<IMapPackageCallback>() {
                @Override
                public void accept(IMapPackageCallback callback) {
                    callback.onMapClickLabel(mapTypeId, pLabels);
                }
            });
        }
    }

    @Override
    public void onMapMove(MapType mapTypeId, long px, long py, boolean moveEnd) {
        if (callbacks.containsKey(mapTypeId)) {
            callbacks.get(mapTypeId).forEach(new Consumer<IMapPackageCallback>() {
                @Override
                public void accept(IMapPackageCallback callback) {
                    callback.onMapMove(mapTypeId, px, py, moveEnd);
                }
            });
        }
    }

    @Override
    public void onMapScaleChanged(MapType mapTypeId, int currentScale) {
        if (callbacks.containsKey(mapTypeId)) {
            callbacks.get(mapTypeId).forEach(new Consumer<IMapPackageCallback>() {
                @Override
                public void accept(IMapPackageCallback callback) {
                    callback.onMapScaleChanged(mapTypeId, currentScale);
                }
            });
        }
    }

    @Override
    public void onMapLoadSuccess(MapType mapTypeId) {
        if (callbacks.containsKey(mapTypeId)) {
            callbacks.get(mapTypeId).forEach(new Consumer<IMapPackageCallback>() {
                @Override
                public void accept(IMapPackageCallback callback) {
                    callback.onMapLoadSuccess(mapTypeId);
                }
            });
        }
    }

    @Override
    public void onMapTouchEvent(MapType mapTypeId, MotionEvent touchEvent) {
        if (callbacks.containsKey(mapTypeId)) {
            callbacks.get(mapTypeId).forEach(new Consumer<IMapPackageCallback>() {
                @Override
                public void accept(IMapPackageCallback callback) {
                    callback.onMapTouchEvent(mapTypeId, touchEvent);
                }
            });
        }
    }

    @Override
    public void onMapClickPoi(MapType mapTypeId, PoiInfoEntity poiInfo) {
        if (callbacks.containsKey(mapTypeId)) {
            callbacks.get(mapTypeId).forEach(new Consumer<IMapPackageCallback>() {
                @Override
                public void accept(IMapPackageCallback callback) {
                    callback.onMapClickPoi(mapTypeId, poiInfo);
                }
            });
        }
    }

    @Override
    public void onOpenLayer(MapType mapTypeId, PoiInfoEntity poiInfo) {
        if (callbacks.containsKey(mapTypeId)) {
            callbacks.get(mapTypeId).forEach(new Consumer<IMapPackageCallback>() {
                @Override
                public void accept(IMapPackageCallback callback) {
                    callback.onQueryTrafficEvent(mapTypeId, poiInfo);
                }
            });
        }
    }

    @Override
    public void onReversePoiClick(MapType mapTypeId, PoiInfoEntity poiInfo) {
        if (callbacks.containsKey(mapTypeId)) {
            callbacks.get(mapTypeId).forEach(new Consumer<IMapPackageCallback>() {
                @Override
                public void accept(IMapPackageCallback callback) {
                    callback.onReversePoiClick(mapTypeId, poiInfo);
                }
            });
        }
    }

    @Override
    public void onMapModeChange(MapType mapTypeId, MapMode mapMode) {
        if (callbacks.containsKey(mapTypeId)) {
            callbacks.get(mapTypeId).forEach(new Consumer<IMapPackageCallback>() {
                @Override
                public void accept(IMapPackageCallback callback) {
                    callback.onMapModeChange(mapTypeId, mapMode);
                }
            });
        }
    }

    @Override
    public void onEGLScreenshot(MapType mapTypeId, byte[] bytes) {
        if (callbacks.containsKey(mapTypeId)) {
            callbacks.get(mapTypeId).forEach(new Consumer<IMapPackageCallback>() {
                @Override
                public void accept(IMapPackageCallback callback) {
                    callback.onEGLScreenshot(mapTypeId, bytes);
                }
            });
        }
    }

    public void updateUiStyle(MapType mapTypeId, ThemeType type) {
        mMapAdapter.updateUiStyle(mapTypeId, type);
    }

    // 返回当前比例尺所表示的地理长度（单位：米）
    public int getCurrentZoomScale(MapType mapTypeId) {
        return mMapAdapter.getCurrentZoomScale(mapTypeId);
    }

    /***
     * 设置TMC
     * @param mapTypeId
     * @param isOpen
     * @return true means success
     */
    public boolean setTrafficStates(MapType mapTypeId, boolean isOpen) {
        return mMapAdapter.setTrafficStates(mapTypeId, isOpen);
    }

    public boolean setTrafficStatesWithoutNetwork(MapType mapTypeId, boolean isOpen) {
        return mMapAdapter.setTrafficStatesWithoutNetwork(mapTypeId, isOpen);
    }

    /**
     * 地图POI分类控制显隐
     *
     * @param mapTypeId
     * @param typeList
     * @param isOpen
     */
    public void setCustomLabelTypeVisible(MapType mapTypeId, ArrayList<Integer> typeList, boolean isOpen) {
        mMapAdapter.setCustomLabelTypeVisible(mapTypeId, typeList, isOpen);
    }

    /**
     * 语音打开HMI界面.
     *
     * @param mapTypeId MapTypeId，对应底图.
     * @param bundle    Bundle，传递打开界面后执行需要的参数.
     */
    public void voiceOpenHmiPage(MapType mapTypeId, Bundle bundle) {
        ThreadManager.getInstance().postUi(() -> {
            if (callbacks.containsKey(mapTypeId) && callbacks.get(mapTypeId) != null) {
                List<IMapPackageCallback> callbacks = this.callbacks.get(mapTypeId);
                if (ConvertUtils.isEmpty(callbacks)) return;
                for (IMapPackageCallback callback : callbacks) {
                    if (null != callback) {
                        callback.onVoiceOpenPage(mapTypeId, bundle);
                    }
                }
            }
        });
    }
}

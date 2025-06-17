package com.fy.navi.service.adapter.map;

import android.view.MotionEvent;

import com.fy.navi.service.define.bean.MapLabelItemBean;
import com.fy.navi.service.define.map.MapMode;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.search.PoiInfoEntity;

import java.util.ArrayList;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/26
 */
public interface IMapAdapterCallback {
    default void onMapLoadSuccess(MapType mapTypeId) {
    }

    default void onMapCenterChanged(MapType mapTypeId, double lon, double lat) {
    }

    default void onMapLevelChanged(MapType mapTypeId, float mapLevel) {
    }

    default void onMapClickBlank(MapType mapTypeId, float px, float py) {
    }

    default void onMapClickLabel(MapType mapTypeId, ArrayList<MapLabelItemBean> pLabels) {
    }

    default void onMapMove(MapType mapTypeId, long px, long py, boolean moveEnd) {
    }

    default void onMapScaleChanged(MapType mapTypeId, int currentScale) {
    }

    default void onMapTouchEvent(MapType mapTypeId, MotionEvent touchEvent) {
    }

    default void onMapClickPoi(MapType mapTypeId, PoiInfoEntity poiInfo) {
    }

    default void onOpenLayer(MapType mapTypeId, PoiInfoEntity poiInfo) {
    }

    default void onReversePoiClick(MapType mapTypeId, PoiInfoEntity poiInfo) {
    }

    default void onMapModeChange(MapType mapTypeId, MapMode mapMode) {
    }

    /**
     * @param isEnterPreview true:进入预览模式，false:退出预览模式
     */
    default void isEnterPreview(MapType mapTypeId, boolean isEnterPreview) {
    }

    default void onEGLScreenshot(MapType mapTypeId, byte[] bytes) {
    }
}

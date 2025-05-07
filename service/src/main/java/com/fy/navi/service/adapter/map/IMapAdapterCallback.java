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
    void onMapInitSuccess(MapType mapTypeId, boolean success);

    void onMapLoadSuccess(MapType mapTypeId);

    void onMapCenterChanged(MapType mapTypeId, double lon, double lat);

    void onMapLevelChanged(MapType mapTypeId, float mapLevel);

    void onMapClickBlank(MapType mapTypeId, float px, float py);

    void onMapClickLabel(MapType mapTypeId, ArrayList<MapLabelItemBean> pLabels);

    void onMapMove(MapType mapTypeId, long px, long py, boolean moveEnd);

    void onMapScaleChanged(MapType mapTypeId, int currentScale);

    void onMapTouchEvent(MapType mapTypeId, MotionEvent touchEvent);

    void onMapClickPoi(MapType mapTypeId, PoiInfoEntity poiInfo);

    void onOpenLayer(MapType mapTypeId, PoiInfoEntity poiInfo);

    void onReversePoiClick(MapType mapTypeId, PoiInfoEntity poiInfo);

    void onMapModeChange(MapType mapTypeId, MapMode mapMode);

    /**
     * @param isEnterPreview true:进入预览模式，false:退出预览模式
     */
    void isEnterPreview(boolean isEnterPreview);

    void onEGLScreenshot(MapType mapTypeId, byte[] bytes);
}

package com.fy.navi.service.adapter.map;

import android.view.MotionEvent;

import com.fy.navi.service.define.bean.MapLabelItemBean;
import com.fy.navi.service.define.map.MapMode;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.search.PoiInfoEntity;

import java.util.ArrayList;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/26
 */
public interface IMapAdapterCallback {
    void onMapInitSuccess(MapTypeId mapTypeId, boolean success);

    void onMapLoadSuccess(MapTypeId mapTypeId);

    void onMapCenterChanged(MapTypeId mapTypeId, double lon, double lat);

    void onMapLevelChanged(MapTypeId mapTypeId, float mapLevel);

    void onMapClickBlank(MapTypeId mapTypeId, float px, float py);

    void onMapClickLabel(MapTypeId mapTypeId, ArrayList<MapLabelItemBean> pLabels);

    void onMapMove(MapTypeId mapTypeId, long px, long py, boolean moveEnd);

    void onMapScaleChanged(MapTypeId mapTypeId, int currentScale);

    void onMapTouchEvent(MapTypeId mapTypeId, MotionEvent touchEvent);

    void onMapClickPoi(MapTypeId mapTypeId, PoiInfoEntity poiInfo);

    void onOpenLayer(MapTypeId mapTypeId, PoiInfoEntity poiInfo);

    void onReversePoiClick(MapTypeId mapTypeId, PoiInfoEntity poiInfo);

    void onMapModeChange(MapTypeId mapTypeId, MapMode mapMode);
}

package com.fy.navi.service.logicpaket.map;

import android.os.Bundle;
import android.view.MotionEvent;

import com.fy.navi.service.define.bean.MapLabelItemBean;
import com.fy.navi.service.define.layer.GemBaseLayer;
import com.fy.navi.service.define.layer.GemLayerItem;
import com.fy.navi.service.define.map.MapMode;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.search.PoiInfoEntity;

import java.util.ArrayList;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/26
 */
public interface IMapPackageCallback {
    default void onMapCenterChanged(MapTypeId mapTypeId, double lon, double lat) {}

    default void onMapLevelChanged(MapTypeId mapTypeId, float mapLevel) {}

    default void onMapClickBlank(MapTypeId mapTypeId, float px, float py) {}

    default void onMapClickLabel(MapTypeId mapTypeId, ArrayList<MapLabelItemBean> pLabels){}

    default void onMapMove(MapTypeId mapTypeId, long px, long py, boolean moveEnd) {}

    default void onMapScaleChanged(MapTypeId mapTypeId, int currentScale) {}

    default void onMapInitSuccess(MapTypeId mapTypeId, boolean success) {}

    default void onMapLoadSuccess(MapTypeId mapTypeId) {}

    default void onMapTouchEvent(MapTypeId mapTypeId, MotionEvent touchEvent) {}

    default void onMapClickPoi(MapTypeId mapTypeId, PoiInfoEntity poiInfo) {}

    default void onReversePoiClick(MapTypeId mapTypeId, PoiInfoEntity poiInfo) {}

    default void onQueryTrafficEvent(MapTypeId mapTypeId, PoiInfoEntity poiInfo) {}

    default void onMapModeChange(MapTypeId mapTypeId, MapMode mapMode) {}

    default void onNaviStatusChange(String naviStatus) {}

    /*UiMode改变的时候触发，暂时就是“黑夜”和“白天”模式的切换*/
    default void onUiModeChanged(int uiMode) {}

    default void onNotifyClick(MapTypeId mapTypeId, GemBaseLayer layer, GemLayerItem pItem) {}

    //语音通过MapPackage触发此接口，传递打开页面的参数.
    default void onVoiceOpenPage(MapTypeId mapTypeId, Bundle bundle) {}
}

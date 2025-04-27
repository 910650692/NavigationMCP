package com.fy.navi.service.logicpaket.map;

import android.os.Bundle;
import android.view.MotionEvent;

import com.fy.navi.service.define.bean.MapLabelItemBean;
import com.fy.navi.service.define.map.MapMode;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.map.ThemeType;
import com.fy.navi.service.define.search.PoiInfoEntity;

import java.util.ArrayList;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/26
 */
public interface IMapPackageCallback {
    default void onMapCenterChanged(MapType mapTypeId, double lon, double lat) {}

    default void onMapLevelChanged(MapType mapTypeId, float mapLevel) {}

    default void onMapClickBlank(MapType mapTypeId, float px, float py) {}

    default void onMapClickLabel(MapType mapTypeId, ArrayList<MapLabelItemBean> pLabels){}

    default void onMapMove(MapType mapTypeId, long px, long py, boolean moveEnd) {}

    default void onMapScaleChanged(MapType mapTypeId, int currentScale) {}

    default void onMapInitSuccess(MapType mapTypeId, boolean success) {}

    default void onMapLoadSuccess(MapType mapTypeId) {}

    default void onMapTouchEvent(MapType mapTypeId, MotionEvent touchEvent) {}

    default void onMapClickPoi(MapType mapTypeId, PoiInfoEntity poiInfo) {}

    default void onReversePoiClick(MapType mapTypeId, PoiInfoEntity poiInfo) {}

    default void onQueryTrafficEvent(MapType mapTypeId, PoiInfoEntity poiInfo) {}

    default void onMapModeChange(MapType mapTypeId, MapMode mapMode) {}

    default void onNaviStatusChange(String naviStatus) {}

    /*UiMode改变的时候触发，暂时就是“黑夜”和“白天”模式的切换*/
    default void onUiModeChanged(ThemeType uiMode) {}

    //语音通过MapPackage触发此接口，传递打开页面的参数.
    default void onVoiceOpenPage(MapType mapTypeId, Bundle bundle) {}

}

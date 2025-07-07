package com.sgm.navi.service.logicpaket.map;

import android.os.Bundle;
import android.view.MotionEvent;

import com.sgm.navi.service.define.bean.MapLabelItemBean;
import com.sgm.navi.service.define.map.MapMode;
import com.sgm.navi.service.define.map.MapNotifyType;
import com.sgm.navi.service.define.map.MapScreenShotDataInfo;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.map.ThemeType;
import com.sgm.navi.service.define.search.PoiInfoEntity;

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

    default void onMove(MapType mapTypeId, long px, long py) {}

    default void onMapScaleChanged(MapType mapTypeId, int currentScale) {}

    default void onMapLoadSuccess(MapType mapTypeId) {}

    default void onMapTouchEvent(MapType mapTypeId, MotionEvent touchEvent) {}

    default void onMapClickPoi(MapType mapTypeId, PoiInfoEntity poiInfo) {}

    default void onReversePoiClick(MapType mapTypeId, PoiInfoEntity poiInfo) {}

    default void onQueryTrafficEvent(MapType mapTypeId, PoiInfoEntity poiInfo) {}

    default void onMapModeChange(MapType mapTypeId, MapMode mapMode) {}

    /*UiMode改变的时候触发，暂时就是“黑夜”和“白天”模式的切换*/
    default void onUiModeChanged(ThemeType uiMode) {}

    //语音通过MapPackage触发此接口，传递打开页面的参数.
    default void onVoiceOpenPage(MapType mapTypeId, Bundle bundle) {}

    default void onEGLScreenshot(MapType mapTypeId, byte[] bytes, MapScreenShotDataInfo info) {}

    default void onNotifyMap(MapType mapTypeId, MapNotifyType eventType) {}
}

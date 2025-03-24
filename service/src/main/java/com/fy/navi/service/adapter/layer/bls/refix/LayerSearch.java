package com.fy.navi.service.adapter.layer.bls.refix;


import android.content.Context;

import com.android.utils.log.Logger;
import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.layer.model.BizLineBusinessInfo;
import com.autonavi.gbl.layer.model.BizPointBusinessInfo;
import com.autonavi.gbl.layer.model.BizPolygonBusinessInfo;
import com.autonavi.gbl.layer.model.BizSearchAlongWayPoint;
import com.autonavi.gbl.layer.model.BizSearchBeginEndPoint;
import com.autonavi.gbl.layer.model.BizSearchChargeStationInfo;
import com.autonavi.gbl.layer.model.BizSearchChildPoint;
import com.autonavi.gbl.layer.model.BizSearchExitEntrancePoint;
import com.autonavi.gbl.layer.model.BizSearchParentPoint;
import com.autonavi.gbl.layer.model.BizSearchType;
import com.autonavi.gbl.map.MapView;
import com.fy.navi.service.adapter.layer.bls.refix.style.SearchLayerStyleAdapter;
import com.fy.navi.service.define.layer.GemLayerClickBusinessType;
import com.fy.navi.service.define.layer.refix.LayerItemBase;
import com.fy.navi.service.define.layer.refix.LayerItemSearchVia;
import com.fy.navi.service.define.layer.refix.LayerItemSearchChild;
import com.fy.navi.service.define.layer.refix.LayerItemSearchEntrance;
import com.fy.navi.service.define.layer.refix.LayerItemSearchParent;

import java.util.ArrayList;

public class LayerSearch extends BaseLayerImpl<SearchLayerStyleAdapter> {

    public LayerSearch(BizControlService bizService, MapView mapView, Context context) {
        super(bizService, mapView, context);
        getLayerSearchControl().setStyle(this);
        getLayerSearchControl().addClickObserver(this);
    }

    public void setSelect(GemLayerClickBusinessType type, String strID, boolean bFocus) {
        Logger.d(TAG, "setSelect", "strID:" + strID, "bFocus:" + bFocus);
        if (getLayerSearchControl() != null) {
            switch (type) {
                case BizSearchTypePoiParentPoint -> {
                    int result = getLayerSearchControl().setFocus(BizSearchType.BizSearchTypePoiParentPoint, strID, bFocus);
                    Logger.d(TAG, "setSelect-BizSearchTypePoiParentPoint:" + result);
                }
                case BizSearchTypePoiChildPoint -> {
                    int result = getLayerSearchControl().setFocus(BizSearchType.BizSearchTypePoiChildPoint, strID, bFocus);
                    Logger.d(TAG, "setSelect-BizSearchTypePoiChildPoint:" + result);
                }
            }
        }
    }


    @Override
    protected SearchLayerStyleAdapter createStyleAdapter() {
        return new SearchLayerStyleAdapter();
    }

    /* 搜索线图层业务 */
    public boolean updateSearchLine(ArrayList<LayerItemBase> vecLineInfo) {
        clearAllItems();
        ArrayList<BizLineBusinessInfo> pointListBl = new ArrayList<BizLineBusinessInfo>();

        getLayerSearchControl().updateSearchLine(pointListBl);
        return true;
    }

    /* 搜索区域图层业务，多区域面 */
    public boolean updateSearchPolygon(ArrayList<LayerItemBase> polygonInfo) {
        ArrayList<BizPolygonBusinessInfo> pointListBl = new ArrayList<BizPolygonBusinessInfo>();

        getLayerSearchControl().updateSearchPolygon(pointListBl);
        return true;
    }

    /* 搜索POI父点图层业务*/
    public boolean updateSearchParentPoi(ArrayList<LayerItemSearchParent> pointList) {
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiParentPoint, true);
        ArrayList<BizSearchParentPoint> pointListBl = new ArrayList<>();

        return getLayerSearchControl().updateSearchParentPoi(pointListBl);
    }

    /* 搜索POI子节点图层业务 */
    public boolean updateSearchChildPoi(ArrayList<LayerItemSearchChild> pointList) {
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiChildPoint, true);
        ArrayList<BizSearchChildPoint> pointListBl = new ArrayList<>();

        return getLayerSearchControl().updateSearchChildPoi(pointListBl);
    }

    /* 搜索POI中心点图层业务 */
    public boolean updateSearchCentralPoi(ArrayList<LayerItemBase> pointList) {
        clearAllItems();
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiCentralPos, true);
        ArrayList<BizPointBusinessInfo> pointListBl = new ArrayList<>();

        return getLayerSearchControl().updateSearchCentralPoi(pointListBl);
    }

    /* 搜索POI出入口图层业务*/
    public boolean updateSearchExitEntrancePoi(ArrayList<LayerItemSearchEntrance> pointList) {
        clearAllItems();
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiExitEntrance, true);
        ArrayList<BizSearchExitEntrancePoint> pointListBl = new ArrayList<>();

        return getLayerSearchControl().updateSearchExitEntrancePoi(pointListBl);
    }

    /* 搜索POI起点、终点、途经点图层业务 */
    public boolean updateSearchBeginEndPoi(ArrayList<LayerItemSearchVia> pointList) {
        clearAllItems();
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiBeginEnd, true);
        ArrayList<BizSearchBeginEndPoint> pointListBl = new ArrayList<>();

        return getLayerSearchControl().updateSearchBeginEndPoi(pointListBl);
    }

    /* 沿途搜索图层业务 */
    public boolean updateSearchAlongRoutePoi(ArrayList<BizSearchAlongWayPoint> pointList) {
        clearAllItems();
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiAlongRoute, true);
        ArrayList<BizSearchAlongWayPoint> pointListBl = new ArrayList<>();

        return getLayerSearchControl().updateSearchAlongRoutePoi(pointListBl);
    }

    /* 沿途搜索气泡图层业务 */
    public boolean updateSearchAlongRoutePoiPop(ArrayList<BizSearchAlongWayPoint> pointList) {
        clearAllItems();
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiAlongRoutePop, true);
        ArrayList<BizSearchAlongWayPoint> pointListBl = new ArrayList<>();

        return getLayerSearchControl().updateSearchAlongRoutePoiPop(pointListBl);
    }

    /* 停车场图层业务 */
    public boolean updateSearchParkPoi(ArrayList<LayerItemBase> pointList) {
        clearAllItems();
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiParkRoute, true);
        ArrayList<BizPointBusinessInfo> pointListBl = new ArrayList<>();

        return getLayerSearchControl().updateSearchParkPoi(pointListBl);
    }

    /* POI扎标图层业务 */
    public boolean updateSearchPoiLabel(LayerItemBase labelInfo) {
        clearAllItems();
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiLabel, true);
        BizPointBusinessInfo pointListBl = new BizPointBusinessInfo();

        return getLayerSearchControl().updateSearchPoiLabel(pointListBl);
    }

    /* 充电桩扎标图层业务 */
    public boolean updateSearchChargeStation(ArrayList<BizSearchChargeStationInfo> pointList) {
        clearAllItems();
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypeChargeStation, true);
        ArrayList<BizSearchChargeStationInfo> pointListBl = new ArrayList<>();

        return getLayerSearchControl().updateSearchChargeStation(pointListBl);
    }

    /* 清除*/
    public void clearAllItems() {
        getLayerSearchControl().clearAllItems();
        getLayerSearchControl().setVisible(false);
    }
}

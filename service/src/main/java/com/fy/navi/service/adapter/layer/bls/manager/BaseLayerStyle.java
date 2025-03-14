package com.fy.navi.service.adapter.layer.bls.manager;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.common.model.Coord3DDouble;
import com.autonavi.gbl.layer.BizAreaControl;
import com.autonavi.gbl.layer.BizCarControl;
import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.layer.BizGuideEagleEyeControl;
import com.autonavi.gbl.layer.BizGuideRouteControl;
import com.autonavi.gbl.layer.BizLabelControl;
import com.autonavi.gbl.layer.BizRoadCrossControl;
import com.autonavi.gbl.layer.BizRoadFacilityControl;
import com.autonavi.gbl.layer.BizSearchControl;
import com.autonavi.gbl.layer.BizUserControl;
import com.autonavi.gbl.layer.RouteJamPointLayerItem;
import com.autonavi.gbl.layer.RoutePathPointItem;
import com.autonavi.gbl.layer.RouteTrafficEventTipsLayerItem;
import com.autonavi.gbl.layer.SearchChildLayerItem;
import com.autonavi.gbl.layer.SearchParentLayerItem;
import com.autonavi.gbl.layer.model.BizRouteType;
import com.autonavi.gbl.layer.model.BizSearchType;
import com.autonavi.gbl.map.MapView;
import com.autonavi.gbl.map.layer.BaseLayer;
import com.autonavi.gbl.map.layer.LayerItem;
import com.autonavi.gbl.map.layer.PointLayerItem;
import com.autonavi.gbl.map.layer.model.ClickViewIdInfo;
import com.autonavi.gbl.map.layer.observer.ILayerClickObserver;
import com.fy.navi.service.adapter.layer.ILayerAdapterCallBack;
import com.fy.navi.service.define.layer.GemBaseLayer;
import com.fy.navi.service.define.layer.GemClickViewIdInfo;
import com.fy.navi.service.define.layer.GemLayerClickBusinessType;
import com.fy.navi.service.define.layer.GemLayerItem;
import com.fy.navi.service.define.map.MapTypeId;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/9
 */
public class BaseLayerStyle implements ILayerClickObserver {
    private static final String TAG = "BaseLayerStyle";
    protected BizControlService mBizService;
    protected MapView mMapView;
    protected BizCarControl mBziCarControl;
    protected BizSearchControl mBziSearchControl;
    protected BizGuideRouteControl mBziRouteControl;
    protected BizGuideEagleEyeControl mGuideEagleEyeControl;
    protected BizRoadCrossControl mBizRoadCrossControl;
    protected BizLabelControl mBizLabelControl;
    protected BizAreaControl mBizAreaControl;
    protected BizRoadFacilityControl mRoadFacilityControl;
    protected BizUserControl mBizUserControl;
    protected final Hashtable<MapTypeId, List<ILayerAdapterCallBack>> layerClickObservers = new Hashtable<>();

    protected BaseLayerStyle(BizControlService bizService, MapView mapView) {
        this.mBizService = bizService;
        this.mMapView = mapView;
        mBziCarControl = mBizService.getBizCarControl(mapView);
        mBziSearchControl = mBizService.getBizSearchControl(mapView);
        mBziRouteControl = mBizService.getBizGuideRouteControl(mapView);
        mGuideEagleEyeControl = mBizService.getBizGuideEagleEyeControl(mapView.getDeviceId());
        mBizRoadCrossControl = mBizService.getBizRoadCrossControl(mapView);
        mBizLabelControl = mBizService.getBizLabelControl(mapView);
        mBizAreaControl = mBizService.getBizAreaControl(mapView);
        mRoadFacilityControl = mBizService.getBizRoadFacilityControl(mapView);
        mBizUserControl = mBizService.getBizUserControl(mapView);
    }

    public void registerLayerObserver(MapTypeId mapTypeId, ILayerAdapterCallBack observer) {
        Logger.d(TAG, "registerLayerObserver");
        List<ILayerAdapterCallBack> callBacks = layerClickObservers.get(mapTypeId);
        if (callBacks == null) {
            callBacks = new ArrayList<>();
            layerClickObservers.put(mapTypeId, callBacks);
        }
        if (!callBacks.contains(observer)) {
            Logger.d(TAG, "registerLayerObserver success!");
            callBacks.add(observer);
        }
    }

    public void unRegisterLayerObserver(MapTypeId mapTypeId, ILayerAdapterCallBack observer) {
        Logger.d(TAG, "unRegisterLayerObserver");
        List<ILayerAdapterCallBack> callBacks = layerClickObservers.get(mapTypeId);
        if (ConvertUtils.isEmpty(callBacks)) return;
        callBacks.remove(observer);
    }

    @Override
    public void onNotifyClick(BaseLayer layer, LayerItem pItem, ClickViewIdInfo clickViewIds) {
        ILayerClickObserver.super.onNotifyClick(layer, pItem, clickViewIds);
        Logger.d(TAG, "onNotifyClick:" + pItem.getBusinessType(), "size:" + layerClickObservers.size());
        layerClickObservers.forEach((mapTypeId, adapterCallBacks) -> {
            adapterCallBacks.forEach(callBack -> {
                callBack.onNotifyClick(mapTypeId, getGemBaseLayer(layer), getGemLayerItem(pItem, clickViewIds));
            });
        });
    }

    @Override
    public void onBeforeNotifyClick(BaseLayer layer, LayerItem pItem, ClickViewIdInfo clickViewIds) {
        ILayerClickObserver.super.onBeforeNotifyClick(layer, pItem, clickViewIds);
        layerClickObservers.forEach((mapTypeId, value) -> {
            value.forEach(callBack -> {
                callBack.onBeforeNotifyClick(mapTypeId, getGemBaseLayer(layer), getGemLayerItem(pItem, clickViewIds));
            });
        });
    }

    @Override
    public void onAfterNotifyClick(BaseLayer layer, LayerItem pItem, ClickViewIdInfo clickViewIds) {
        ILayerClickObserver.super.onAfterNotifyClick(layer, pItem, clickViewIds);
        layerClickObservers.forEach((mapTypeId, value) -> {
            value.forEach(callBack -> {
                callBack.onAfterNotifyClick(mapTypeId, getGemBaseLayer(layer), getGemLayerItem(pItem, clickViewIds));
            });
        });
    }

    protected void unInit() {

    }

    // TODO 根据业务实现，暂时不需要
    private GemBaseLayer getGemBaseLayer(BaseLayer baseLayer) {
        GemBaseLayer gemBaseLayer = new GemBaseLayer();
        return gemBaseLayer;
    }

    // TODO 这里只转换了部分已知道的需求
    private GemLayerItem getGemLayerItem(LayerItem pItem, ClickViewIdInfo clickViewIds) {
        GemLayerItem gemLayerItem = new GemLayerItem();

        GemClickViewIdInfo gemClickViewIdInfo = new GemClickViewIdInfo();
        gemClickViewIdInfo.setBgMarkerClickViewId(clickViewIds.bgMarkerClickViewId);
        gemClickViewIdInfo.setPoiMarkerClickViewId(clickViewIds.poiMarkerClickViewId);
        gemClickViewIdInfo.setBubbleMarkerClickViewId(clickViewIds.bubbleMarkerClickViewId);
        gemLayerItem.setClickViewIdInfo(gemClickViewIdInfo);

        switch (pItem.getBusinessType()) {
            // 搜索图层内容点击
            case BizSearchType.BizSearchTypePoiParentPoint -> {
                Coord3DDouble coord3DDouble = ((SearchParentLayerItem) pItem).getPosition();
                gemLayerItem.setLat(coord3DDouble.lat);
                gemLayerItem.setLog(coord3DDouble.lon);
                gemLayerItem.setZ(coord3DDouble.z);
                gemLayerItem.setClickBusinessType(GemLayerClickBusinessType.BizSearchTypePoiParentPoint);
                gemLayerItem.setLayerItemId(((SearchParentLayerItem) pItem).getMIndex() - 1);
            }
            case BizSearchType.BizSearchTypePoiChildPoint -> {
                Coord3DDouble coord3DDouble = ((SearchChildLayerItem) pItem).getPosition();
                gemLayerItem.setLat(coord3DDouble.lat);
                gemLayerItem.setLog(coord3DDouble.lon);
                gemLayerItem.setZ(coord3DDouble.z);
                gemLayerItem.setClickBusinessType(GemLayerClickBusinessType.BizSearchTypePoiChildPoint);
            }

            // TODO 路线图层内容点击
            case BizRouteType.BizRouteTypePath -> {
                long index = getIndex(pItem.getID());
                if (index >= 0) {
                    gemLayerItem.setLayerItemId(index);
                    gemLayerItem.setClickBusinessType(GemLayerClickBusinessType.BizRouteTypePath);
                }
            }
            case BizRouteType.BizRouteTypeStartPoint -> {
                Coord3DDouble coord3DDouble = ((RoutePathPointItem) pItem).getPosition();
                gemLayerItem.setLat(coord3DDouble.lat);
                gemLayerItem.setLog(coord3DDouble.lon);
                gemLayerItem.setZ(coord3DDouble.z);
                gemLayerItem.setClickBusinessType(GemLayerClickBusinessType.BizRouteTypeStartPoint);
            }
            case BizRouteType.BizRouteTypeEndPoint -> {
                Coord3DDouble coord3DDouble = ((RoutePathPointItem) pItem).getPosition();
                gemLayerItem.setLat(coord3DDouble.lat);
                gemLayerItem.setLog(coord3DDouble.lon);
                gemLayerItem.setZ(coord3DDouble.z);
                gemLayerItem.setClickBusinessType(GemLayerClickBusinessType.BizRouteTypeEndPoint);
            }
            case BizRouteType.BizRouteTypeWeather -> {
                long index = getIndex(pItem.getID());
                if (index >= 0) {
                    gemLayerItem.setLayerItemId(index);
                    gemLayerItem.setClickBusinessType(GemLayerClickBusinessType.BizRouteTypeWeather);
                }
            }
            case BizRouteType.BizRouteTypeRestArea -> {
                long index = getIndex(pItem.getID());
                if (index >= 0) {
                    gemLayerItem.setLayerItemId(index);
                    gemLayerItem.setClickBusinessType(GemLayerClickBusinessType.BizRouteTypeRestArea);
                }
            }
            case BizRouteType.BizRouteTypeViaChargeStationPoint -> {
                long index = getIndex(pItem.getID());
                if (index >= 0) {
                    gemLayerItem.setLayerItemId(index);
                    gemLayerItem.setClickBusinessType(GemLayerClickBusinessType.BizRouteTypeViaChargeStationPoint);
                }
            }
            case BizRouteType.BizRouteTypeTrafficEventTip -> {
                RouteTrafficEventTipsLayerItem trafficEventTipsLayerItem = (RouteTrafficEventTipsLayerItem) pItem;
                long eventID = trafficEventTipsLayerItem.getMTrafficEventTipsInfo().mTrafficIncident.ID;
                Coord3DDouble coord3DDouble = trafficEventTipsLayerItem.getPosition();
                gemLayerItem.setLat(coord3DDouble.lat);
                gemLayerItem.setLog(coord3DDouble.lon);
                gemLayerItem.setZ(coord3DDouble.z);
                gemLayerItem.setEventId(eventID);
                gemLayerItem.setClickBusinessType(GemLayerClickBusinessType.BizRouteTypeTrafficEventTip);
            }
            case BizRouteType.BizRouteTypeJamPoint -> {
                // TODO 参数待验证
                RouteJamPointLayerItem trafficEventTipsLayerItem = (RouteJamPointLayerItem) pItem;
                long eventID = Long.parseLong(trafficEventTipsLayerItem.getID());
                Coord3DDouble coord3DDouble = trafficEventTipsLayerItem.getPosition();
                gemLayerItem.setLat(coord3DDouble.lat);
                gemLayerItem.setLog(coord3DDouble.lon);
                gemLayerItem.setZ(coord3DDouble.z);
                gemLayerItem.setEventId(eventID);
                gemLayerItem.setClickBusinessType(GemLayerClickBusinessType.BizRouteTypeJamPoint);
            }
            case BizSearchType.BizSearchTypePoiParkRoute -> {
                //停车场poi点击
                PointLayerItem pointLayerItem = (PointLayerItem) pItem;
                gemLayerItem.setLayerItemId(getIndex(pointLayerItem.getID()));
            }
            default -> {
                // TODO 扩展新的需求
                gemLayerItem.setClickBusinessType(GemLayerClickBusinessType.UnKnown);
            }

        }
        return gemLayerItem;
    }

    private long getIndex(String indexStr) {
        long flag = -1;
        try {
            flag = Long.parseLong(indexStr);
        } catch (Exception e) {
            Logger.e(TAG, "error:" + e.getMessage());
        }
        return flag;
    }
}

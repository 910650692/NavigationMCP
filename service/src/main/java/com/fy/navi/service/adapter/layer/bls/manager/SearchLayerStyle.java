package com.fy.navi.service.adapter.layer.bls.manager;

import static com.autonavi.gbl.layer.model.BizSearchType.BizSearchTypePoiParkRoute;

import com.android.utils.ConvertUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.common.model.Coord3DDouble;
import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.layer.model.BizPointBusinessInfo;
import com.autonavi.gbl.layer.model.BizSearchChildPoint;
import com.autonavi.gbl.layer.model.BizSearchParentPoint;
import com.autonavi.gbl.layer.model.BizSearchType;
import com.autonavi.gbl.layer.model.PoiParentType;
import com.autonavi.gbl.map.MapView;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.layer.GemLayerClickBusinessType;
import com.fy.navi.service.define.layer.SearchResultLayer;
import com.fy.navi.service.define.navi.NaviParkingEntity;

import java.util.ArrayList;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/9
 */
public class SearchLayerStyle extends BaseLayerStyle {
    private static final String TAG = "SearchLayerStyle";
    public static final int INFORMATION_TYPE_UNKONWN = 0;
    public static final int INFORMATION_TYPE_GAS = 1;
    public static final int INFORMATION_TYPE_CHARGE = 2;
    public static final int INFORMATION_TYPE_CAR_WASHING = 3;
    public static final int INFORMATION_TYPE_FOOD = 4;
    public static final int INFORMATION_TYPE_PARKING = 5;
    public static final int INFORMATION_TYPE_SCENIC = 6;
    public static final int INFORMATION_TYPE_SERVICE_ZONE = 7;

    public SearchLayerStyle(BizControlService bizService, MapView mapView) {
        super(bizService, mapView);
    }

    public void addLayerClickListener() {
        mBziSearchControl.addClickObserver(this);
    }

    public void removeLayerClickListener() {
        mBziSearchControl.removeClickObserver(this);
    }

    public void updateSearchPoiLayer(SearchResultLayer searchResultLayer) {
        Logger.d(TAG, "搜索结果扎标参数", searchResultLayer);
        clearSearchAllItemLayer();
        ArrayList<SearchResultLayer.ParentPoint> parentPoints = searchResultLayer.getParentPoints();
        if (!ConvertUtils.isEmpty(parentPoints)) updateSearchParentPoi(parentPoints);
        ArrayList<SearchResultLayer.ChildPoint> childPoints = searchResultLayer.getChildPoints();
        if (!ConvertUtils.isEmpty(childPoints)) updateSearchChildPoi(childPoints);
    }

    /**
     * 更新搜索结果父节点扎标
     *
     * @param parentPointArrayList 节点参数
     */
    public void updateSearchParentPoi(ArrayList<SearchResultLayer.ParentPoint> parentPointArrayList) {
        mBziSearchControl.setVisible(BizSearchType.BizSearchTypePoiParentPoint, true);
        ArrayList<BizSearchParentPoint> parentPoints = new ArrayList<>();
        for (SearchResultLayer.ParentPoint point : parentPointArrayList) {
            BizSearchParentPoint parent = new BizSearchParentPoint();
            parent.id = point.id;
            parent.poiName = point.poiName;
            parent.mPos3D.lat = point.mPos3D.lat;
            parent.mPos3D.lon = point.mPos3D.lon;
            parent.index = point.index;
            parent.typeCode = point.typeCode;
            parent.poiType = point.poiType;

            parentPoints.add(parent);
        }
//        ArrayList<BizSearchParentPoint> parentPoints = (ArrayList<BizSearchParentPoint>) GsonUtils.fromJson2List(parentPointArrayList, BizSearchParentPoint.class);
        Logger.d(TAG, "搜索结果扎标参数", parentPoints);

        mBziSearchControl.updateSearchParentPoi(parentPoints);
    }

    public void updateSearchChildPoi(ArrayList<SearchResultLayer.ChildPoint> childPointArrayList) {
        mBziSearchControl.setVisible(BizSearchType.BizSearchTypePoiChildPoint, true);
/*        ArrayList<BizSearchChildPoint> childPoints = new ArrayList<>();
        for (SearchResultLayer.ChildPoint point : childPointArrayList) {
            BizSearchChildPoint childPoint = new BizSearchChildPoint();
            childPoint.childType = point.childType;
            childPoint.shortName = point.shortName;
            childPoint.mPos3D.lon = point.mPos3D.lon;
            childPoint.mPos3D.lat = point.mPos3D.lat;
            childPoints.add(childPoint);
        }*/
        ArrayList<BizSearchChildPoint> childPoints = (ArrayList<BizSearchChildPoint>) GsonUtils.fromJson2List(childPointArrayList, BizSearchChildPoint.class);
        mBziSearchControl.updateSearchChildPoi(childPoints);
    }

    public void clearSearchAllItemLayer() {
        if (mBziSearchControl == null) return;
        Logger.d(TAG, "清除搜索图层");
        mBziSearchControl.setVisible(BizSearchType.BizSearchTypePoiParentPoint, false);
        mBziSearchControl.setVisible(BizSearchType.BizSearchTypePoiChildPoint, false);
        mBziSearchControl.clearAllItems(BizSearchType.BizSearchTypePoiParentPoint);
        mBziSearchControl.clearAllItems(BizSearchType.BizSearchTypePoiChildPoint);
    }

    public void clearSearchMarks() {
        mBziSearchControl.clearAllItems(BizSearchType.BizSearchTypePoiLabel);
        mBziSearchControl.setVisible(BizSearchType.BizSearchTypePoiLabel, false);
    }

    public boolean updateSearchPoiLabel(SearchResultLayer.ChildPoint childPoint) {
        if (mBziSearchControl == null) return false;
        // 清楚上一次的扎点
        clearSearchMarks();
        // 新的扎点开始设置
        mBziSearchControl.setVisible(BizSearchType.BizSearchTypePoiLabel, true);
        BizPointBusinessInfo bizBusinessInfos = new BizPointBusinessInfo();
        bizBusinessInfos.id = childPoint.mPid;
        bizBusinessInfos.mPos3D = new Coord3DDouble(childPoint.mPos3D.getLon(), childPoint.mPos3D.getLat(), childPoint.mPos3D.getZ());
        bizBusinessInfos.mTypeCode = childPoint.mTypeCode;
        return mBziSearchControl.updateSearchPoiLabel(bizBusinessInfos);
    }

    @Override
    protected void unInit() {
        removeLayerClickListener();
        if (!ConvertUtils.isEmpty(mBziSearchControl)) mBziSearchControl = null;
    }

    public void updateSearchParkPoi(ArrayList<NaviParkingEntity> parkList) {
        if (!ConvertUtils.isEmpty(parkList)) {
            ArrayList<BizPointBusinessInfo> list = new ArrayList<>();
            for (NaviParkingEntity poiInfoEntity : parkList) {
                GeoPoint point = poiInfoEntity.getPoint();
                BizPointBusinessInfo bizPointBusinessInfo = new BizPointBusinessInfo();
                bizPointBusinessInfo.mPos3D = new Coord3DDouble(point.getLon(), point.getLat(), point.getZ());
                list.add(bizPointBusinessInfo);
            }
            if (mBziSearchControl != null) {
                mBziSearchControl.updateSearchParkPoi(list);
            }
        }
    }

    public void clearSearchParkPoi() {
        if (mBziSearchControl != null) {
            mBziSearchControl.clearAllItems(BizSearchType.BizSearchTypePoiParkRoute);
        }
    }

    public void setParkFocus(String strID, boolean bFocus) {
        if (mBziSearchControl != null) {
            mBziSearchControl.setFocus(BizSearchTypePoiParkRoute, strID, bFocus);
        }
    }

    public void setSelect(GemLayerClickBusinessType type, String strID, boolean bFocus) {
        Logger.d(TAG, "setSelect", "strID:" + strID, "bFocus:" + bFocus);
        if (mBziSearchControl != null) {
            switch (type) {
                case BizSearchTypePoiParentPoint -> {
                    int result = mBziSearchControl.setFocus(BizSearchType.BizSearchTypePoiParentPoint, strID, bFocus);
                    Logger.d(TAG, "setSelect-BizSearchTypePoiParentPoint:" + result);
                }
                case BizSearchTypePoiChildPoint -> {
                    int result =  mBziSearchControl.setFocus(BizSearchType.BizSearchTypePoiChildPoint, strID, bFocus);
                    Logger.d(TAG, "setSelect-BizSearchTypePoiChildPoint:" + result);
                }
            }
        }
    }
}

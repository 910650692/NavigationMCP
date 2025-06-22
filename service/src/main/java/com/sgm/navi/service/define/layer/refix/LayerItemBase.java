package com.sgm.navi.service.define.layer.refix;

import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.bean.GeoPoint;

import java.util.ArrayList;

import lombok.Getter;
import lombok.Setter;

/**
 * BizPointBusinessInfo
 */
@Setter
@Getter
public class LayerItemBase {

    protected String TAG = MapDefaultFinalTag.LAYER_ITEM_SERVICE_TAG;
    //
    private ArrayList<GeoPoint> vecPoints = new ArrayList();
    //
    private String strID;
    //
    private String typeCode = "";
    //是否聚焦
    private boolean bFocus;

    public void addGeoPoint(GeoPoint geoPoint) {
        vecPoints.add(geoPoint);
    }

    public void addGeoPoints(ArrayList<GeoPoint> geoPoints) {
        vecPoints.addAll(geoPoints);
    }

}

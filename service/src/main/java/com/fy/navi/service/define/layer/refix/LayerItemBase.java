package com.fy.navi.service.define.layer.refix;

import com.fy.navi.service.define.bean.GeoPoint;

import java.util.ArrayList;

import lombok.Getter;
import lombok.Setter;

/**
 * BizPointBusinessInfo
 */
@Setter
@Getter
public class LayerItemBase {

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

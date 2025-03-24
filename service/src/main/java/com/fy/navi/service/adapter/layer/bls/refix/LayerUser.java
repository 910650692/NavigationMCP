package com.fy.navi.service.adapter.layer.bls.refix;

import android.content.Context;

import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.layer.model.BizUserFavoritePoint;
import com.autonavi.gbl.map.MapView;
import com.fy.navi.service.define.map.GmBizUserFavoritePoint;
import com.fy.navi.service.define.map.MapTypeId;

import java.util.ArrayList;
import java.util.List;

public class LayerUser extends BaseLayerImpl {

    public LayerUser(BizControlService bizService, MapView mapView, Context context) {
        super(bizService, mapView, context);
        getLayerUserControl().setStyle(this);
        getLayerUserControl().addClickObserver(this);
        getLayerUserControl().addFocusChangeObserver(this);
    }


    public void updateFavoriteMain(List<GmBizUserFavoritePoint> list) {
        ArrayList<BizUserFavoritePoint> points = new ArrayList<>();
        list.forEach((entity) -> {
            BizUserFavoritePoint point = new BizUserFavoritePoint();
            point.favoriteType = entity.favoriteType;
            point.mPos3D.lat = entity.lat;
            point.mPos3D.lon = entity.lon;
            points.add(point);
        });
        clearFavoriteMain();
        getLayerUserControl().updateFavoriteMain(points);
    }

    public void clearFavoriteMain() {
        getLayerUserControl().clearAllItems();
    }
}

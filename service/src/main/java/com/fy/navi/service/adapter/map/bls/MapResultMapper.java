package com.fy.navi.service.adapter.map.bls;

import com.autonavi.gbl.common.model.Coord2DDouble;
import com.autonavi.gbl.map.OperatorPosture;
import com.autonavi.gbl.map.model.MapLabelItem;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.search.PoiInfoEntity;

public final class MapResultMapper {
    private static class Holder {
        private static final MapResultMapper INSTANCE = new MapResultMapper();
    }

    private MapResultMapper() {
    }

    public static MapResultMapper getInstance() {
        return MapResultMapper.Holder.INSTANCE;
    }


    public PoiInfoEntity mapFrom(MapLabelItem item) {
        PoiInfoEntity poi = new PoiInfoEntity();
        Coord2DDouble coord2DDouble = OperatorPosture.mapToLonLat(item.pixel20X, item.pixel20Y);
        GeoPoint point = new GeoPoint();
        point.setLon(coord2DDouble.lon);
        point.setLat(coord2DDouble.lat);
        poi.setPoint(point);
        poi.setName(item.name);
        poi.setPid(item.poiid);
        return poi;
    }

}

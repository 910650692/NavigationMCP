package com.sgm.navi.service.define.user.msgpush;

public class MobileRouteViaPointInfo {
    // 途径点类型 0：GPS坐标；1：手动选点；2：POI坐标
    public int type;
    // 途径点POI名称
    public String name;
    // 途径点POI的经度
    public String lon;
    // 途径点POI的纬度
    public String lat;
    // 途径点POI的ID
    public String poiId;
    // 途径点POI的TypeCode
    public String typeCode;

    // 途经点投影到的link id
    public String link_id;

    public String getLink_id() {
        return link_id;
    }

    public void setLink_id(String link_id) {
        this.link_id = link_id;
    }

    public int getType() {
        return type;
    }

    public void setType(int type) {
        this.type = type;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getLon() {
        return lon;
    }

    public void setLon(String lon) {
        this.lon = lon;
    }

    public String getLat() {
        return lat;
    }

    public void setLat(String lat) {
        this.lat = lat;
    }

    public String getPoiId() {
        return poiId;
    }

    public void setPoiId(String poiId) {
        this.poiId = poiId;
    }

    public String getTypeCode() {
        return typeCode;
    }

    public void setTypeCode(String typeCode) {
        this.typeCode = typeCode;
    }
}

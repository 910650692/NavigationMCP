package com.fy.navi.service.define.route;

import com.fy.navi.service.define.search.PoiInfoEntity;

import java.io.Serializable;
import java.util.ArrayList;

public class RouteMsgPushInfo implements Serializable {
    private String Name;
    private Object msgPushInfo;
    private PoiInfoEntity poiInfoEntity;
    private RoutePoint startPoint;
    private RoutePoint endPoint;
    private ArrayList<RoutePoint> viaPoints = new ArrayList<>();
    private ArrayList<PoiInfoEntity> viaPoiInfoEntity = new ArrayList<>();

    public String getName() {
        return Name;
    }

    public void setName(String name) {
        Name = name;
    }

    public Object getMsgPushInfo() {
        return msgPushInfo;
    }

    public void setMsgPushInfo(Object msgPushInfo) {
        this.msgPushInfo = msgPushInfo;
    }

    public PoiInfoEntity getPoiInfoEntity() {
        return poiInfoEntity;
    }

    public void setPoiInfoEntity(PoiInfoEntity poiInfoEntity) {
        this.poiInfoEntity = poiInfoEntity;
    }

    public RoutePoint getStartPoint() {
        return startPoint;
    }

    public void setStartPoint(RoutePoint startPoint) {
        this.startPoint = startPoint;
    }

    public RoutePoint getEndPoint() {
        return endPoint;
    }

    public void setEndPoint(RoutePoint endPoint) {
        this.endPoint = endPoint;
    }

    public ArrayList<RoutePoint> getViaPoints() {
        return viaPoints;
    }

    public void setViaPoints(ArrayList<RoutePoint> viaPoints) {
        this.viaPoints = viaPoints;
    }

    public ArrayList<PoiInfoEntity> getViaPoiInfoEntity() {
        return viaPoiInfoEntity;
    }

    public void setViaPoiInfoEntity(ArrayList<PoiInfoEntity> viaPoiInfoEntity) {
        this.viaPoiInfoEntity = viaPoiInfoEntity;
    }
}

package com.fy.navi.service.define.user.forecast;

import com.autonavi.gbl.util.model.Date;
import com.autonavi.gbl.util.model.Time;
import com.fy.navi.service.define.bean.GeoPoint;

import java.util.ArrayList;

public class OftenArrivedItemInfo {

    public String wstrPoiID;
    public String wstrPoiName;
    public String wstrPoiType;
    public String wstrAddress;
    public GeoPoint stDisplayCoord;
    public GeoPoint stNaviCoord;
    public String parent;
    public String towardsAngle;
    public String floorNo;
    public int childType;
    public int endPoiExtension;
    public long topTime;
    public ArrayList<Long> uTimeList;

    public Date date;
    public Time time;

    public String getWstrPoiID() {
        return wstrPoiID;
    }

    public void setWstrPoiID(String wstrPoiID) {
        this.wstrPoiID = wstrPoiID;
    }

    public String getWstrPoiName() {
        return wstrPoiName;
    }

    public void setWstrPoiName(String wstrPoiName) {
        this.wstrPoiName = wstrPoiName;
    }

    public String getWstrPoiType() {
        return wstrPoiType;
    }

    public void setWstrPoiType(String wstrPoiType) {
        this.wstrPoiType = wstrPoiType;
    }

    public String getWstrAddress() {
        return wstrAddress;
    }

    public void setWstrAddress(String wstrAddress) {
        this.wstrAddress = wstrAddress;
    }

    public GeoPoint getStDisplayCoord() {
        return stDisplayCoord;
    }

    public void setStDisplayCoord(GeoPoint stDisplayCoord) {
        this.stDisplayCoord = stDisplayCoord;
    }

    public GeoPoint getStNaviCoord() {
        return stNaviCoord;
    }

    public void setStNaviCoord(GeoPoint stNaviCoord) {
        this.stNaviCoord = stNaviCoord;
    }

    public String getParent() {
        return parent;
    }

    public void setParent(String parent) {
        this.parent = parent;
    }

    public String getTowardsAngle() {
        return towardsAngle;
    }

    public void setTowardsAngle(String towardsAngle) {
        this.towardsAngle = towardsAngle;
    }

    public String getFloorNo() {
        return floorNo;
    }

    public void setFloorNo(String floorNo) {
        this.floorNo = floorNo;
    }

    public int getChildType() {
        return childType;
    }

    public void setChildType(int childType) {
        this.childType = childType;
    }

    public int getEndPoiExtension() {
        return endPoiExtension;
    }

    public void setEndPoiExtension(int endPoiExtension) {
        this.endPoiExtension = endPoiExtension;
    }

    public long getTopTime() {
        return topTime;
    }

    public void setTopTime(long topTime) {
        this.topTime = topTime;
    }

    public ArrayList<Long> getuTimeList() {
        return uTimeList;
    }

    public void setuTimeList(ArrayList<Long> uTimeList) {
        this.uTimeList = uTimeList;
    }

    public Date getDate() {
        return date;
    }

    public void setDate(Date date) {
        this.date = date;
    }

    public Time getTime() {
        return time;
    }

    public void setTime(Time time) {
        this.time = time;
    }
}

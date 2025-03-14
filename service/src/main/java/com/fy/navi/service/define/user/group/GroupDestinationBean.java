package com.fy.navi.service.define.user.group;

import com.fy.navi.service.define.bean.GeoPoint;

import java.util.ArrayList;

public class GroupDestinationBean {

    public String name;
    public String poiId;
    public String address;
    public String poiType;
    public String newType;
    public String cityCode;
    public String phoneNumber;
    public GeoPoint poiLoc;
    public GeoPoint display;
    public String industry;
    public String towardsAngle;
    public ArrayList<GeoPoint> entranceList;
    public ArrayList<GeoPoint> exitList;
    public String parent;
    public String floorNo;
    public String childType;
    public String naviExtCode;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getPoiId() {
        return poiId;
    }

    public void setPoiId(String poiId) {
        this.poiId = poiId;
    }

    public String getAddress() {
        return address;
    }

    public void setAddress(String address) {
        this.address = address;
    }

    public String getPoiType() {
        return poiType;
    }

    public void setPoiType(String poiType) {
        this.poiType = poiType;
    }

    public String getNewType() {
        return newType;
    }

    public void setNewType(String newType) {
        this.newType = newType;
    }

    public String getCityCode() {
        return cityCode;
    }

    public void setCityCode(String cityCode) {
        this.cityCode = cityCode;
    }

    public String getPhoneNumber() {
        return phoneNumber;
    }

    public void setPhoneNumber(String phoneNumber) {
        this.phoneNumber = phoneNumber;
    }

    public GeoPoint getPoiLoc() {
        return poiLoc;
    }

    public void setPoiLoc(GeoPoint poiLoc) {
        this.poiLoc = poiLoc;
    }

    public GeoPoint getDisplay() {
        return display;
    }

    public void setDisplay(GeoPoint display) {
        this.display = display;
    }

    public String getIndustry() {
        return industry;
    }

    public void setIndustry(String industry) {
        this.industry = industry;
    }

    public String getTowardsAngle() {
        return towardsAngle;
    }

    public void setTowardsAngle(String towardsAngle) {
        this.towardsAngle = towardsAngle;
    }

    public ArrayList<GeoPoint> getEntranceList() {
        return entranceList;
    }

    public void setEntranceList(ArrayList<GeoPoint> entranceList) {
        this.entranceList = entranceList;
    }

    public ArrayList<GeoPoint> getExitList() {
        return exitList;
    }

    public void setExitList(ArrayList<GeoPoint> exitList) {
        this.exitList = exitList;
    }

    public String getParent() {
        return parent;
    }

    public void setParent(String parent) {
        this.parent = parent;
    }

    public String getFloorNo() {
        return floorNo;
    }

    public void setFloorNo(String floorNo) {
        this.floorNo = floorNo;
    }

    public String getChildType() {
        return childType;
    }

    public void setChildType(String childType) {
        this.childType = childType;
    }

    public String getNaviExtCode() {
        return naviExtCode;
    }

    public void setNaviExtCode(String naviExtCode) {
        this.naviExtCode = naviExtCode;
    }
}

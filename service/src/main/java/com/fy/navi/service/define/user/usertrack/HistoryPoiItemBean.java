package com.fy.navi.service.define.user.usertrack;

import com.fy.navi.service.define.bean.GeoPoint;

import java.util.ArrayList;


public class HistoryPoiItemBean {

    private String mItemId;
    // POI唯一标识ID
    private String mPoiId;
    // POI类别编码
    private String mTypeCode;
    // POI名称
    private String mName;
    // POI地址
    private String mAddress;
    // POI坐标
    private GeoPoint mPoiLoc;
    private GeoPoint mNavLoc;
    // 终点的父POIID
    private String mParent;
    // 终点的父POI与子POI的关系类型
    private int mChildType;
    // POI门脸朝向
    private String mTowardsAngle;
    // 终点的楼层信息
    private String mFloorNo;
    // 该POI有扩展信息 0x0001:标识multi_navi多到达点，强制查询POI 0x0002:标示shop_mark底商，强制查询POI
    private int mEndPoiExtension;
    // 更新时间
    private long mUpdateTime;
    // 城市编码
    private int mCityCode;
    // 城市名称
    private String mCityName;
    // 入口位置列表
    private ArrayList<GeoPoint> mEntranceList;


    public String getItemId() {
        return mItemId;
    }

    public void setItemId(final String itemId) {
        this.mItemId = itemId;
    }

    public String getPoiId() {
        return mPoiId;
    }

    public void setPoiId(final String poiId) {
        this.mPoiId = poiId;
    }

    public String getTypeCode() {
        return mTypeCode;
    }

    public void setTypeCode(final String typeCode) {
        this.mTypeCode = typeCode;
    }

    public String getName() {
        return mName;
    }

    public void setName(final String name) {
        this.mName = name;
    }

    public String getAddress() {
        return mAddress;
    }

    public void setAddress(final String address) {
        this.mAddress = address;
    }

    public GeoPoint getPoiLoc() {
        return mPoiLoc;
    }

    public void setPoiLoc(final GeoPoint poiLoc) {
        this.mPoiLoc = poiLoc;
    }

    public GeoPoint getNavLoc() {
        return mNavLoc;
    }

    public void setNavLoc(final GeoPoint navLoc) {
        this.mNavLoc = navLoc;
    }

    public String getParent() {
        return mParent;
    }

    public void setParent(final String parent) {
        this.mParent = parent;
    }

    public int getChildType() {
        return mChildType;
    }

    public void setChildType(final int childType) {
        this.mChildType = childType;
    }

    public String getTowardsAngle() {
        return mTowardsAngle;
    }

    public void setTowardsAngle(final String towardsAngle) {
        this.mTowardsAngle = towardsAngle;
    }

    public String getFloorNo() {
        return mFloorNo;
    }

    public void setFloorNo(final String floorNo) {
        this.mFloorNo = floorNo;
    }

    public int getEndPoiExtension() {
        return mEndPoiExtension;
    }

    public void setEndPoiExtension(final int endPoiExtension) {
        this.mEndPoiExtension = endPoiExtension;
    }

    public long getUpdateTime() {
        return mUpdateTime;
    }

    public void setUpdateTime(final long updateTime) {
        this.mUpdateTime = updateTime;
    }

    public int getCityCode() {
        return mCityCode;
    }

    public void setCityCode(final int cityCode) {
        this.mCityCode = cityCode;
    }

    public String getCityName() {
        return mCityName;
    }

    public void setCityName(final String cityName) {
        this.mCityName = cityName;
    }

    public ArrayList<GeoPoint> getEntranceList() {
        return mEntranceList;
    }

    public void setEntranceList(final ArrayList<GeoPoint> entranceList) {
        this.mEntranceList = entranceList;
    }
}

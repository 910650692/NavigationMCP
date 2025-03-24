package com.fy.navi.service.define.user.forecast;

import com.autonavi.gbl.util.model.Date;
import com.autonavi.gbl.util.model.Time;
import com.fy.navi.service.define.bean.GeoPoint;

import java.util.ArrayList;

public class OftenArrivedItemInfo {

    private String mWstrPoiID;
    private String mWstrPoiName;
    private String mWstrPoiType;
    private String mWstrAddress;
    private GeoPoint mStDisplayCoord;
    private GeoPoint mStNaviCoord;
    private String mParent;
    private String mTowardsAngle;
    private String mFloorNo;
    private int mChildType;
    private int mEndPoiExtension;
    private long mTopTime;
    private ArrayList<Long> mTimeList;

    private Date mDate;
    private Time mTime;

    public String getWstrPoiID() {
        return mWstrPoiID;
    }

    public void setWstrPoiID(final String wstrPoiID) {
        this.mWstrPoiID = wstrPoiID;
    }

    public String getWstrPoiName() {
        return mWstrPoiName;
    }

    public void setWstrPoiName(final String wstrPoiName) {
        this.mWstrPoiName = wstrPoiName;
    }

    public String getWstrPoiType() {
        return mWstrPoiType;
    }

    public void setWstrPoiType(final String wstrPoiType) {
        this.mWstrPoiType = wstrPoiType;
    }

    public String getWstrAddress() {
        return mWstrAddress;
    }

    public void setWstrAddress(final String wstrAddress) {
        this.mWstrAddress = wstrAddress;
    }

    public GeoPoint getStDisplayCoord() {
        return mStDisplayCoord;
    }

    public void setStDisplayCoord(final GeoPoint stDisplayCoord) {
        this.mStDisplayCoord = stDisplayCoord;
    }

    public GeoPoint getStNaviCoord() {
        return mStNaviCoord;
    }

    public void setStNaviCoord(final GeoPoint stNaviCoord) {
        this.mStNaviCoord = stNaviCoord;
    }

    public String getParent() {
        return mParent;
    }

    public void setParent(final String parent) {
        this.mParent = parent;
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

    public int getChildType() {
        return mChildType;
    }

    public void setChildType(final int childType) {
        this.mChildType = childType;
    }

    public int getEndPoiExtension() {
        return mEndPoiExtension;
    }

    public void setEndPoiExtension(final int endPoiExtension) {
        this.mEndPoiExtension = endPoiExtension;
    }

    public long getTopTime() {
        return mTopTime;
    }

    public void setTopTime(final long topTime) {
        this.mTopTime = topTime;
    }

    public ArrayList<Long> getTimeList() {
        return mTimeList;
    }

    public void setTimeList(final ArrayList<Long> timeList) {
        this.mTimeList = timeList;
    }

    public Date getDate() {
        return mDate;
    }

    public void setDate(final Date date) {
        this.mDate = date;
    }

    public Time getTime() {
        return mTime;
    }

    public void setTime(final Time time) {
        this.mTime = time;
    }
}

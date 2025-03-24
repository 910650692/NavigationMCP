package com.fy.navi.service.define.user.usertrack;

import com.fy.navi.service.define.bean.GeoPoint;

import java.util.ArrayList;



public class HistoryRouteItemBean {
    private String mItemId;
    private GeoPoint mStartLoc;
    private GeoPoint mEndLoc;
    private int mMethod;
    private HistoryPoiItemBean mFromPoi;
    private HistoryPoiItemBean mToPoi;
    private ArrayList<HistoryPoiItemBean> mMidPoi;
    private long mUpdateTime;

    private int mType;
    private String mId;

    public String getItemId() {
        return mItemId;
    }

    public void setItemId(final String itemId) {
        this.mItemId = itemId;
    }

    public GeoPoint getStartLoc() {
        return mStartLoc;
    }

    public void setStartLoc(final GeoPoint startLoc) {
        this.mStartLoc = startLoc;
    }

    public GeoPoint getEndLoc() {
        return mEndLoc;
    }

    public void setEndLoc(final GeoPoint endLoc) {
        this.mEndLoc = endLoc;
    }

    public int getMethod() {
        return mMethod;
    }

    public void setMethod(final int method) {
        this.mMethod = method;
    }

    public HistoryPoiItemBean getFromPoi() {
        return mFromPoi;
    }

    public void setFromPoi(final HistoryPoiItemBean fromPoi) {
        this.mFromPoi = fromPoi;
    }

    public HistoryPoiItemBean getToPoi() {
        return mToPoi;
    }

    public void setToPoi(final HistoryPoiItemBean toPoi) {
        this.mToPoi = toPoi;
    }

    public ArrayList<HistoryPoiItemBean> getMidPoi() {
        return mMidPoi;
    }

    public void setMidPoi(final ArrayList<HistoryPoiItemBean> midPoi) {
        this.mMidPoi = midPoi;
    }

    public long getUpdateTime() {
        return mUpdateTime;
    }

    public void setUpdateTime(final long updateTime) {
        this.mUpdateTime = updateTime;
    }

    public int getType() {
        return mType;
    }

    public void setType(final int type) {
        this.mType = type;
    }

    public String getId() {
        return mId;
    }

    public void setId(final String id) {
        this.mId = id;
    }
}

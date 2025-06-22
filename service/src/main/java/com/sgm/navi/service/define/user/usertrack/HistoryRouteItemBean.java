package com.sgm.navi.service.define.user.usertrack;

import com.sgm.navi.service.define.bean.GeoPoint;

import java.util.ArrayList;
import java.util.Date;


public class HistoryRouteItemBean {
    private String mItemId;
    // 起点位置
    private GeoPoint mStartLoc;
    // 终点位置
    private GeoPoint mEndLoc;
    // 路线策略
    private int mMethod;
    // 起点信息
    private HistoryPoiItemBean mFromPoi;
    // 终点信息
    private HistoryPoiItemBean mToPoi;
    // 途经点信息
    private ArrayList<HistoryPoiItemBean> mMidPoi;
    // 更新时间，单位秒
    private long mUpdateTime;
    // 更新时间（本地使用）
    private Date mTime;

    // 车机端只有 302 类型
    private int mType;
    // 唯一ID
    private String mId;

    private boolean mIsCompleted;

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

    public boolean getIsCompleted() {
        return mIsCompleted;
    }

    public void setIsCompleted(final boolean isCompleted) {
        this.mIsCompleted = isCompleted;
    }

    public Date getTime() {
        return mTime;
    }

    public void setTime(final Date time) {
        this.mTime = time;
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

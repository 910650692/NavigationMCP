package com.sgm.navi.service.define.navi;

import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.route.ChargingInfo;
import com.sgm.navi.service.define.utils.NumberUtils;

public class NaviViaEntity {
    private String mPid;
    private GeoPoint mRealPos; // 实际位置坐标(必选)
    private String mName; // POI名称(可选)
    private String mAddress; // 地址
    private ChargingInfo mChargeInfo; // 充电桩信息
    private String mDistance;
    private String mArriveTime;
    private String mArriveDay;
    private long mArriveTimeStamp;
    //到达剩余电量（百分比）非实时有数据，不能直接使用
    private int mArriveBatteryLeft = -255;
    private boolean mIsUserAdd;//是否手动添加的途经点
    boolean mIsEndPoi; // 是否是终点POI

    public String getArriveDay() {
        return mArriveDay;
    }

    public void setArriveDay(final String arriveDay) {
        this.mArriveDay = arriveDay;
    }

    public GeoPoint getRealPos() {
        return mRealPos;
    }

    public void setRealPos(final GeoPoint realPos) {
        this.mRealPos = realPos;
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

    public ChargingInfo getChargeInfo() {
        return mChargeInfo;
    }

    public void setChargeInfo(final ChargingInfo chargeInfo) {
        this.mChargeInfo = chargeInfo;
    }

    public String getDistance() {
        return mDistance;
    }

    public void setDistance(final String distance) {
        this.mDistance = distance;
    }

    public String getArriveTime() {
        return mArriveTime;
    }

    public void setArriveTime(final String arriveTime) {
        this.mArriveTime = arriveTime;
    }

    public String getPid() {
        return mPid;
    }

    public void setPid(final String pid) {
        this.mPid = pid;
    }

    public int getArriveBatteryLeft() {
        return mArriveBatteryLeft;
    }
    public void setArriveBatteryLeft(int arriveBatteryLeft) {
        this.mArriveBatteryLeft = arriveBatteryLeft;
    }

    public boolean isUserAdd() {
        return mIsUserAdd;
    }

    public void setIsUserAdd(boolean isUserAdd) {
        this.mIsUserAdd = isUserAdd;
    }

    public boolean isEndPoi() {
        return mIsEndPoi;
    }

    public void setIsEndPoi(boolean isEndPoi) {
        this.mIsEndPoi = isEndPoi;
    }

    @Override
    public String toString() {
        return "NaviViaEntity{" +
                "mPid='" + mPid + '\'' +
                ", mRealPos=" + mRealPos +
                ", mName='" + mName + '\'' +
                ", mAddress='" + mAddress + '\'' +
                ", mChargeInfo=" + mChargeInfo +
                ", mDistance='" + mDistance + '\'' +
                ", mArriveTime='" + mArriveTime + '\'' +
                ", mArriveDay='" + mArriveDay + '\'' +
                ", mArriveTimeStamp=" + mArriveTimeStamp +
                ", mArriveBatteryLeft=" + mArriveBatteryLeft +
                ", mIsUserAdd=" + mIsUserAdd +
                '}';
    }

    public long getmArriveTimeStamp() {
        return mArriveTimeStamp;
    }

    public void setmArriveTimeStamp(long mArriveTimeStamp) {
        this.mArriveTimeStamp = mArriveTimeStamp;
    }
}

package com.fy.navi.mapservice.bean.common;

import android.os.Parcel;
import android.os.Parcelable;


public class BaseTurnInfo implements Parcelable {

    private int mType;
    private int mAllTime; //剩余距离/m
    private int mAllDist; //剩余时间/s
    private String mCurRouteName = ""; //当前道路名
    private int mCurRoadClass; //当前道路等级
    /**
     * 当前路口转向ID 用来区分显示tbt板上的转向提示图标
     * 官方枚举类是 ManeuverIconID
     * 本地枚举类是INaviConstant.ManeuverId.
     */
    private int mCurManeuverID = -1;
    private String mNextRouteName = ""; //下个道路名
    private int mNextManeuverID = -1; //下个路口转向ID
    private int mNextDist; //下个路口距离/m
    private int mRingOutCnt; //剩余红绿灯数量

    public BaseTurnInfo() {

    }

    public static final Creator<BaseTurnInfo> CREATOR = new Creator<BaseTurnInfo>() {
        @Override
        public BaseTurnInfo createFromParcel(final Parcel source) {
            return new BaseTurnInfo(source);
        }

        @Override
        public BaseTurnInfo[] newArray(final int size) {
            return new BaseTurnInfo[size];
        }
    };

    public BaseTurnInfo (final Parcel in) {
        mType = in.readInt();
        mAllTime = in.readInt();
        mAllDist = in.readInt();
        mCurRouteName = in.readString();
        mCurRoadClass = in.readInt();
        mCurManeuverID = in.readInt();
        mNextRouteName = in.readString();
        mNextManeuverID = in.readInt();
        mNextDist = in.readInt();
        mRingOutCnt = in.readInt();
    }


    @Override
    public void writeToParcel(final Parcel dest, final int flags) {
        dest.writeInt(mType);
        dest.writeInt(mAllTime);
        dest.writeInt(mAllDist);
        dest.writeString(mCurRouteName);
        dest.writeInt(mCurRoadClass);
        dest.writeInt(mCurManeuverID);
        dest.writeString(mNextRouteName);
        dest.writeInt(mNextManeuverID);
        dest.writeInt(mNextDist);
        dest.writeInt(mRingOutCnt);
    }


    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public String toString() {
        return "BaseTurnInfo{" +
                "mType=" + mType +
                ", mAllTime=" + mAllTime +
                ", mAllDist=" + mAllDist +
                ", mCurRouteName='" + mCurRouteName + '\'' +
                ", mCurRoadClass=" + mCurRoadClass +
                ", mCurManeuverID=" + mCurManeuverID +
                ", mNextRouteName='" + mNextRouteName + '\'' +
                ", mNextManeuverID=" + mNextManeuverID +
                ", mNextDist=" + mNextDist +
                ", mRingOutCnt=" + mRingOutCnt +
                '}';
    }

    public int getType() {
        return mType;
    }

    public void setType(final int type) {
        mType = type;
    }

    public int getAllTime() {
        return mAllTime;
    }

    public void setAllTime(final int allTime) {
        mAllTime = allTime;
    }

    public int getAllDist() {
        return mAllDist;
    }

    public void setAllDist(final int allDist) {
        mAllDist = allDist;
    }

    public String getCurRouteName() {
        return mCurRouteName;
    }

    public void setCurRouteName(final String curRouteName) {
        mCurRouteName = curRouteName;
    }

    public int getCurRoadClass() {
        return mCurRoadClass;
    }

    public void setCurRoadClass(final int curRoadClass) {
        mCurRoadClass = curRoadClass;
    }

    public int getCurManeuverID() {
        return mCurManeuverID;
    }

    public void setCurManeuverID(final int curManeuverID) {
        mCurManeuverID = curManeuverID;
    }

    public String getNextRouteName() {
        return mNextRouteName;
    }

    public void setNextRouteName(final String nextRouteName) {
        mNextRouteName = nextRouteName;
    }

    public int getNextManeuverID() {
        return mNextManeuverID;
    }

    public void setNextManeuverID(final int nextManeuverID) {
        mNextManeuverID = nextManeuverID;
    }

    public int getNextDist() {
        return mNextDist;
    }

    public void setNextDist(final int nextDist) {
        mNextDist = nextDist;
    }

    public int getRingOutCnt() {
        return mRingOutCnt;
    }

    public void setRingOutCnt(final int ringOutCnt) {
        mRingOutCnt = ringOutCnt;
    }

}
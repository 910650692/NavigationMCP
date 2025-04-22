package com.fy.navi.mapservice.bean.common;

import android.os.Parcel;
import android.os.Parcelable;

public class BaseRouteLine implements Parcelable {

    private long mPathID; //路线ID
    private int mType;
    private String mLabel; //路线标签
    private long mDistance;//数字长度
    private String mLength; //格式化之后的路线长度，带单位
    private String mTravelTime;//格式化之后的路线耗时，xx:xx
    private String mStaticTravelTime;//静态到达时间
    private String mTollCost;//路线总收费金额
    private String mTrafficLightCount;//红绿灯个数
    private String mNaviID;//导航标识
    private boolean mIsOnline;//是否在线算路
    private boolean mElecRouteBool; //是否电车算路
    private boolean mCanBeArrive; //电车算路是否可到达

    public BaseRouteLine() {
    }

    public BaseRouteLine(final Parcel in) {
        mPathID = in.readLong();
        mType = in.readInt();
        mLabel = in.readString();
        mLength = in.readString();
        mDistance = in.readLong();
        mTravelTime = in.readString();
        mStaticTravelTime = in.readString();
        mTollCost = in.readString();
        mTrafficLightCount = in.readString();
        mNaviID = in.readString();
        mIsOnline = in.readByte() == 1;
        mElecRouteBool = in.readByte() == 1;
        mCanBeArrive = in.readByte() == 1;
    }

    @Override
    public void writeToParcel(final Parcel dest, final int flags) {
        dest.writeLong(mPathID);
        dest.writeInt(mType);
        dest.writeString(mLabel);
        dest.writeString(mLength);
        dest.writeLong(mDistance);
        dest.writeString(mTravelTime);
        dest.writeString(mStaticTravelTime);
        dest.writeString(mTollCost);
        dest.writeString(mTrafficLightCount);
        dest.writeString(mNaviID);
        dest.writeByte((byte) (mIsOnline ? 1 : 0));
        dest.writeByte((byte) (mElecRouteBool ? 1 : 0));
        dest.writeByte((byte) (mCanBeArrive ? 1 : 0));
    }

    public static final Creator<BaseRouteLine> CREATOR = new Creator<BaseRouteLine>() {
        @Override
        public BaseRouteLine createFromParcel(final Parcel source) {
            return new BaseRouteLine(source);
        }

        @Override
        public BaseRouteLine[] newArray(final int size) {
            return new BaseRouteLine[size];
        }
    };



    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public String toString() {
        return "BaseRouteLine{" +
                "mPathID=" + mPathID +
                ", mType=" + mType +
                ", mLabel='" + mLabel + '\'' +
                ", mDistance=" + mDistance +
                ", mLength='" + mLength + '\'' +
                ", mTravelTime='" + mTravelTime + '\'' +
                ", mStaticTravelTime='" + mStaticTravelTime + '\'' +
                ", mTollCost='" + mTollCost + '\'' +
                ", mTrafficLightCount='" + mTrafficLightCount + '\'' +
                ", mNaviID='" + mNaviID + '\'' +
                ", mIsOnline=" + mIsOnline +
                ", mElecRouteBool=" + mElecRouteBool +
                ", mCanBeArrive=" + mCanBeArrive +
                '}';
    }

    public long getPathID() {
        return mPathID;
    }

    public void setPathID(final long pathID) {
        mPathID = pathID;
    }

    public int getType() {
        return mType;
    }

    public void setType(final int type) {
        mType = type;
    }

    public String getLabel() {
        return mLabel;
    }

    public void setLabel(final String label) {
        mLabel = label;
    }

    public long getDistance() {
        return mDistance;
    }

    public void setDistance(final long distance) {
        mDistance = distance;
    }

    public String getLength() {
        return mLength;
    }

    public void setLength(final String length) {
        mLength = length;
    }

    public String getTravelTime() {
        return mTravelTime;
    }

    public void setTravelTime(final String travelTime) {
        mTravelTime = travelTime;
    }

    public String getStaticTravelTime() {
        return mStaticTravelTime;
    }

    public void setStaticTravelTime(final String staticTravelTime) {
        mStaticTravelTime = staticTravelTime;
    }

    public String getTollCost() {
        return mTollCost;
    }

    public void setTollCost(final String tollCost) {
        mTollCost = tollCost;
    }

    public String getTrafficLightCount() {
        return mTrafficLightCount;
    }

    public void setTrafficLightCount(final String trafficLightCount) {
        mTrafficLightCount = trafficLightCount;
    }

    public String getNaviID() {
        return mNaviID;
    }

    public void setNaviID(final String naviID) {
        mNaviID = naviID;
    }

    public boolean isIsOnline() {
        return mIsOnline;
    }

    public void setIsOnline(final boolean isOnline) {
        mIsOnline = isOnline;
    }

    public boolean isElecRouteBool() {
        return mElecRouteBool;
    }

    public void setElecRouteBool(final boolean elecRouteBool) {
        mElecRouteBool = elecRouteBool;
    }

    public boolean isCanBeArrive() {
        return mCanBeArrive;
    }

    public void setCanBeArrive(final boolean canBeArrive) {
        mCanBeArrive = canBeArrive;
    }
}

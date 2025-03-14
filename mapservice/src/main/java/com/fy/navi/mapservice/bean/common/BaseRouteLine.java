package com.fy.navi.mapservice.bean.common;

import android.os.Parcel;
import android.os.Parcelable;

public class BaseRouteLine implements Parcelable {

    private long pathID; //路线ID
    private int type;
    private String label; //路线标签
    private String length; //格式化之后的路线长度，带单位
    private String travelTime;//格式化之后的路线耗时，xx:xx
    private String staticTravelTime;//静态到达时间
    private String tollCost;//路线总收费金额
    private String trafficLightCount;//红绿灯个数
    private String naviID;//导航标识
    private boolean isOnline;//是否在线算路
    private boolean elecRouteBool; //是否电车算路
    private boolean canBeArrive; //电车算路是否可到达

    public BaseRouteLine() {}

    public BaseRouteLine(Parcel in) {
        pathID = in.readLong();
        type = in.readInt();
        label = in.readString();
        length = in.readString();
        travelTime = in.readString();
        staticTravelTime = in.readString();
        tollCost = in.readString();
        trafficLightCount = in.readString();
        naviID = in.readString();
        isOnline = in.readByte() == 1;
        elecRouteBool = in.readByte() == 1;
        canBeArrive = in.readByte() == 1;
    }

    public static final Creator<BaseRouteLine> CREATOR = new Creator<BaseRouteLine>() {
        @Override
        public BaseRouteLine createFromParcel(Parcel source) {
            return new BaseRouteLine(source);
        }

        @Override
        public BaseRouteLine[] newArray(int size) {
            return new BaseRouteLine[size];
        }
    };

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeLong(pathID);
        dest.writeInt(type);
        dest.writeString(label);
        dest.writeString(length);
        dest.writeString(travelTime);
        dest.writeString(staticTravelTime);
        dest.writeString(tollCost);
        dest.writeString(trafficLightCount);
        dest.writeString(naviID);
        dest.writeByte((byte) (isOnline ? 1 : 0));
        dest.writeByte((byte) (elecRouteBool ? 1 : 0));
        dest.writeByte((byte) (canBeArrive ? 1 : 0));
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public String toString() {
        return "BaseRouteLine{" +
                "type=" + type +
                ", label='" + label + '\'' +
                ", length='" + length + '\'' +
                ", travelTime='" + travelTime + '\'' +
                ", staticTravelTime='" + staticTravelTime + '\'' +
                ", tollCost='" + tollCost + '\'' +
                ", trafficLightCount='" + trafficLightCount + '\'' +
                ", naviID='" + naviID + '\'' +
                ", isOnline=" + isOnline +
                '}';
    }

    public int getType() {
        return type;
    }

    public void setType(int type) {
        this.type = type;
    }

    public String getLabel() {
        return label;
    }

    public void setLabel(String label) {
        this.label = label;
    }

    public String getLength() {
        return length;
    }

    public void setLength(String length) {
        this.length = length;
    }

    public String getTravelTime() {
        return travelTime;
    }

    public void setTravelTime(String travelTime) {
        this.travelTime = travelTime;
    }

    public String getStaticTravelTime() {
        return staticTravelTime;
    }

    public void setStaticTravelTime(String staticTravelTime) {
        this.staticTravelTime = staticTravelTime;
    }

    public String getTollCost() {
        return tollCost;
    }

    public void setTollCost(String tollCost) {
        this.tollCost = tollCost;
    }

    public String getTrafficLightCount() {
        return trafficLightCount;
    }

    public void setTrafficLightCount(String trafficLightCount) {
        this.trafficLightCount = trafficLightCount;
    }

    public String getNaviID() {
        return naviID;
    }

    public void setNaviID(String naviID) {
        this.naviID = naviID;
    }

    public boolean isOnline() {
        return isOnline;
    }

    public void setOnline(boolean online) {
        isOnline = online;
    }

}

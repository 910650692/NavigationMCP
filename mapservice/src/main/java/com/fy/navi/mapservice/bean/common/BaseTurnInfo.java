package com.fy.navi.mapservice.bean.common;

import android.os.Parcel;
import android.os.Parcelable;


public class BaseTurnInfo implements Parcelable {

    public int allTime; //剩余距离/m
    public int allDist; //剩余时间/s
    public String curRouteName = ""; //当前道路名
    public int curManeuverID = -1; //当前转向ID
    public String nextRouteName = ""; //下个道路名
    public int nextManeuverID = -1; //下个路口转向ID
    public int nextDist; //下个路口距离/m
    public int curRoadClass; //当前道路等级
    public int routeRemainLightCount; //剩余红绿灯数量
    public int cityCode;

    public BaseTurnInfo() {
        curRoadClass = -1;
        cityCode = 0;
    }

    public BaseTurnInfo (Parcel in) {
        allTime = in.readInt();
        allDist = in.readInt();
        curRouteName = in.readString();
        curManeuverID = in.readInt();
        nextRouteName = in.readString();
        nextManeuverID = in.readInt();
        nextDist = in.readInt();
        curRoadClass = in.readInt();
        routeRemainLightCount = in.readInt();
        cityCode = in.readInt();
    }

    public static final Creator<BaseTurnInfo> CREATOR = new Creator<BaseTurnInfo>() {
        @Override
        public BaseTurnInfo createFromParcel(Parcel source) {
            return new BaseTurnInfo(source);
        }

        @Override
        public BaseTurnInfo[] newArray(int size) {
            return new BaseTurnInfo[size];
        }
    };


    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeInt(allTime);
        dest.writeInt(allDist);
        dest.writeString(curRouteName);
        dest.writeInt(curManeuverID);
        dest.writeString(nextRouteName);
        dest.writeInt(nextManeuverID);
        dest.writeInt(nextDist);
        dest.writeInt(curRoadClass);
        dest.writeInt(routeRemainLightCount);
        dest.writeInt(cityCode);
    }


    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public String toString() {
        return "BaseTurnInfo{" +
                "allTime=" + allTime +
                ", allDist=" + allDist +
                ", curRouteName='" + curRouteName + '\'' +
                ", curManeuverID=" + curManeuverID +
                ", nextRouteName='" + nextRouteName + '\'' +
                ", nextManeuverID=" + nextManeuverID +
                ", nextDist=" + nextDist +
                ", curRoadClass=" + curRoadClass +
                ", remainLightCount=" + routeRemainLightCount +
                ", cityCode=" + cityCode +
                '}';
    }

    public int getAllTime() {
        return allTime;
    }

    public void setAllTime(int allTime) {
        this.allTime = allTime;
    }

    public int getAllDist() {
        return allDist;
    }

    public void setAllDist(int allDist) {
        this.allDist = allDist;
    }

    public String getCurRouteName() {
        return curRouteName;
    }

    public void setCurRouteName(String curRouteName) {
        this.curRouteName = curRouteName;
    }

    public int getCurManeuverID() {
        return curManeuverID;
    }

    public void setCurManeuverID(int curManeuverID) {
        this.curManeuverID = curManeuverID;
    }

    public String getNextRouteName() {
        return nextRouteName;
    }

    public void setNextRouteName(String nextRouteName) {
        this.nextRouteName = nextRouteName;
    }

    public int getNextManeuverID() {
        return nextManeuverID;
    }

    public void setNextManeuverID(int nextManeuverID) {
        this.nextManeuverID = nextManeuverID;
    }

    public int getNextDist() {
        return nextDist;
    }

    public void setNextDist(int nextDist) {
        this.nextDist = nextDist;
    }

    public int getCurRoadClass() {
        return curRoadClass;
    }

    public void setCurRoadClass(int curRoadClass) {
        this.curRoadClass = curRoadClass;
    }

    public int getRouteRemainLightCount() {
        return routeRemainLightCount;
    }

    public void setRouteRemainLightCount(int routeRemainLightCount) {
        this.routeRemainLightCount = routeRemainLightCount;
    }

    public int getCityCode() {
        return cityCode;
    }

    public void setCityCode(int cityCode) {
        this.cityCode = cityCode;
    }

}

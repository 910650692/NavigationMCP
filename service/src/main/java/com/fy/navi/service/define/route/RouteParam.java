package com.fy.navi.service.define.route;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import com.fy.navi.service.define.bean.GeoPoint;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/5
 */
public class RouteParam implements Parcelable {
    private @RoutePoiType.RoutePoiTypeId int poiType; // poi点位类型(必选)
    private GeoPoint realPos; // 实际位置坐标(必选)
    private GeoPoint naviPos; // 导航位置坐标
    private int type;// 行程点类型: 0默认 当前gps位置, 1 手动选择, 2 poi方式(必选) 如果是POI搜索结果，这里一定要填2
    public float sigshelter; // 终端判断的GPS卫星信号强度，浮点型，取值为(0默认,1)，负数表示无效(可选)
    public long roadID; // 道路ID(可选)
    public String poiID; // POI ID(可选)
    public String name; // POI名称(可选)
    public String typeCode; // POI类别编码（如果是POI搜索结果，这里建议填）
    public short fromJump; // 是否第三方跳转行程点 0非跳转；1跳转的
    public short overhead; // 高架标识, 0默认，1高架上，2高架下,
    public float overheadBackAltDiff; // 高架回传高度差（单位是m，默认值-10001.1）
    public int floor; // 建筑楼层ID
    public String extendInfoFlag; // 扩展标识 用于导航服务请求poi信息使用
    public String srcApp; // 第三方应用名称
    public String address; // 地址
    public ChargingInfo chargeInfo; // 充电桩信息
    public String retainParam; // 回传参数
    public int adCode; // 回传参数

    public RouteParam() {

    }

    public RouteParam(Parcel in) {
        poiType = in.readInt();
        realPos = in.readParcelable(GeoPoint.class.getClassLoader());
        naviPos = in.readParcelable(GeoPoint.class.getClassLoader());
        type = in.readInt();
        sigshelter = in.readFloat();
        roadID = in.readLong();
        poiID = in.readString();
        name = in.readString();
        typeCode = in.readString();
        fromJump = (short) in.readInt();
        overhead = (short) in.readInt();
        overheadBackAltDiff = in.readFloat();
        floor = in.readInt();
        extendInfoFlag = in.readString();
        srcApp = in.readString();
        address = in.readString();
        retainParam = in.readString();
        adCode = in.readInt();
    }

    public static final Creator<RouteParam> CREATOR = new Creator<RouteParam>() {
        @Override
        public RouteParam createFromParcel(Parcel in) {
            return new RouteParam(in);
        }

        @Override
        public RouteParam[] newArray(int size) {
            return new RouteParam[size];
        }
    };

    public int getPoiType() {
        return poiType;
    }

    public void setPoiType(int poiType) {
        this.poiType = poiType;
    }

    public GeoPoint getRealPos() {
        return realPos;
    }

    public void setRealPos(GeoPoint realPos) {
        this.realPos = realPos;
    }

    public GeoPoint getNaviPos() {
        return naviPos;
    }

    public void setNaviPos(GeoPoint naviPos) {
        this.naviPos = naviPos;
    }

    public int getType() {
        return type;
    }

    public void setType(int type) {
        this.type = type;
    }

    public float getSigshelter() {
        return sigshelter;
    }

    public void setSigshelter(float sigshelter) {
        this.sigshelter = sigshelter;
    }

    public long getRoadID() {
        return roadID;
    }

    public void setRoadID(long roadID) {
        this.roadID = roadID;
    }

    public String getPoiID() {
        return poiID;
    }

    public void setPoiID(String poiID) {
        this.poiID = poiID;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getTypeCode() {
        return typeCode;
    }

    public void setTypeCode(String typeCode) {
        this.typeCode = typeCode;
    }

    public short getFromJump() {
        return fromJump;
    }

    public void setFromJump(short fromJump) {
        this.fromJump = fromJump;
    }

    public short getOverhead() {
        return overhead;
    }

    public void setOverhead(short overhead) {
        this.overhead = overhead;
    }

    public float getOverheadBackAltDiff() {
        return overheadBackAltDiff;
    }

    public void setOverheadBackAltDiff(float overheadBackAltDiff) {
        this.overheadBackAltDiff = overheadBackAltDiff;
    }

    public int getFloor() {
        return floor;
    }

    public void setFloor(int floor) {
        this.floor = floor;
    }

    public String getExtendInfoFlag() {
        return extendInfoFlag;
    }

    public void setExtendInfoFlag(String extendInfoFlag) {
        this.extendInfoFlag = extendInfoFlag;
    }

    public String getSrcApp() {
        return srcApp;
    }

    public void setSrcApp(String srcApp) {
        this.srcApp = srcApp;
    }

    public String getAddress() {
        return address;
    }

    public void setAddress(String address) {
        this.address = address;
    }

    public ChargingInfo getChargeInfo() {
        return chargeInfo;
    }

    public void setChargeInfo(ChargingInfo chargeInfo) {
        this.chargeInfo = chargeInfo;
    }

    public String getRetainParam() {
        return retainParam;
    }

    public void setRetainParam(String retainParam) {
        this.retainParam = retainParam;
    }

    public int getAdCode() {
        return adCode;
    }

    public void setAdCode(int adCode) {
        this.adCode = adCode;
    }

    @Override
    public String toString() {
        return "RouteParam{" +
                "poiType=" + poiType +
                "realPos=" + realPos +
                ", naviPos=" + naviPos +
                ", type=" + type +
                ", sigshelter=" + sigshelter +
                ", roadID=" + roadID +
                ", poiID='" + poiID + '\'' +
                ", name='" + name + '\'' +
                ", typeCode='" + typeCode + '\'' +
                ", fromJump=" + fromJump +
                ", overhead=" + overhead +
                ", overheadBackAltDiff=" + overheadBackAltDiff +
                ", floor=" + floor +
                ", extendInfoFlag='" + extendInfoFlag + '\'' +
                ", srcApp='" + srcApp + '\'' +
                ", address='" + address + '\'' +
                ", chargeInfo=" + chargeInfo +
                ", retainParam='" + retainParam + '\'' +
                ", adCode='" + adCode + '\'' +
                '}';
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel parcel, int i) {
        parcel.writeInt(poiType);
        parcel.writeParcelable(realPos, i);
        parcel.writeParcelable(naviPos, i);
        parcel.writeInt(type);
        parcel.writeFloat(sigshelter);
        parcel.writeLong(roadID);
        parcel.writeString(poiID);
        parcel.writeString(name);
        parcel.writeString(typeCode);
        parcel.writeInt((int) fromJump);
        parcel.writeInt((int) overhead);
        parcel.writeFloat(overheadBackAltDiff);
        parcel.writeInt(floor);
        parcel.writeString(extendInfoFlag);
        parcel.writeString(srcApp);
        parcel.writeString(address);
        parcel.writeString(retainParam);
        parcel.writeInt(adCode);
    }
}

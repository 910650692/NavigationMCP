package com.fy.navi.service.define.route;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.search.PoiInfoEntity;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RouteParam implements Parcelable {
    private @RoutePoiType.RoutePoiTypeId int mPoiType; // poi点位类型(必选)
    private int mAddressType; // 地点类型: 0默认 , 1 替换补能点, 2 充电站（非补能规划）
    private GeoPoint mRealPos; // 实际位置坐标(必选)
    private GeoPoint mNaviPos; // 导航位置坐标
    private int mType;// 行程点类型: 0默认 当前gps位置, 1 手动选择, 2 poi方式(必选) 如果是POI搜索结果，这里一定要填2
    private float mSigshelter; // 终端判断的GPS卫星信号强度，浮点型，取值为(0默认,1)，负数表示无效(可选)
    private long mRoadID; // 道路ID(可选)
    private String mPoiID; // POI ID(可选)
    private String mName; // POI名称(可选)
    private String mTypeCode; // POI类别编码（如果是POI搜索结果，这里建议填）
    private short mFromJump; // 是否第三方跳转行程点 0非跳转；1跳转的
    private short mOverhead; // 高架标识, 0默认，1高架上，2高架下,
    private float mOverheadBackAltDiff; // 高架回传高度差（单位是m，默认值-10001.1）
    private int mFloor; // 建筑楼层ID
    private String mExtendInfoFlag; // 扩展标识 用于导航服务请求poi信息使用
    private String mSrcApp; // 第三方应用名称
    private String mAddress; // 地址
    private ChargingInfo mChargeInfo; // 充电桩信息
    private String mRetainParam; // 回传参数
    private int mAdCode; // 回传参数
    private PoiInfoEntity mPoiInfoEntity; // poi数据

    public RouteParam() {

    }

    public RouteParam(final Parcel in) {
        mPoiType = in.readInt();
        mRealPos = in.readParcelable(GeoPoint.class.getClassLoader());
        mNaviPos = in.readParcelable(GeoPoint.class.getClassLoader());
        mType = in.readInt();
        mSigshelter = in.readFloat();
        mRoadID = in.readLong();
        mPoiID = in.readString();
        mName = in.readString();
        mTypeCode = in.readString();
        mFromJump = (short) in.readInt();
        mOverhead = (short) in.readInt();
        mOverheadBackAltDiff = in.readFloat();
        mFloor = in.readInt();
        mExtendInfoFlag = in.readString();
        mSrcApp = in.readString();
        mAddress = in.readString();
        mRetainParam = in.readString();
        mAdCode = in.readInt();
        mPoiInfoEntity = in.readParcelable(PoiInfoEntity.class.getClassLoader());
    }

    public static final Creator<RouteParam> CREATOR = new Creator<RouteParam>() {
        @Override
        public RouteParam createFromParcel(final Parcel in) {
            return new RouteParam(in);
        }

        @Override
        public RouteParam[] newArray(final int size) {
            return new RouteParam[size];
        }
    };

    public int getPoiType() {
        return mPoiType;
    }

    public void setPoiType(final int poiType) {
        this.mPoiType = poiType;
    }

    public GeoPoint getRealPos() {
        return mRealPos;
    }

    public void setRealPos(final GeoPoint realPos) {
        this.mRealPos = realPos;
    }

    public GeoPoint getNaviPos() {
        return mNaviPos;
    }

    public void setNaviPos(final GeoPoint naviPos) {
        this.mNaviPos = naviPos;
    }

    public int getType() {
        return mType;
    }

    public void setType(final int type) {
        this.mType = type;
    }

    public float getSigshelter() {
        return mSigshelter;
    }

    public void setSigshelter(final float sigshelter) {
        this.mSigshelter = sigshelter;
    }

    public long getRoadID() {
        return mRoadID;
    }

    public void setRoadID(final long roadID) {
        this.mRoadID = roadID;
    }

    public String getPoiID() {
        return mPoiID;
    }

    public void setPoiID(final String poiID) {
        this.mPoiID = poiID;
    }

    public String getName() {
        return mName;
    }

    public void setName(final String name) {
        this.mName = name;
    }

    public String getTypeCode() {
        return mTypeCode;
    }

    public void setTypeCode(final String typeCode) {
        this.mTypeCode = typeCode;
    }

    public short getFromJump() {
        return mFromJump;
    }

    public void setFromJump(final short fromJump) {
        this.mFromJump = fromJump;
    }

    public short getOverhead() {
        return mOverhead;
    }

    public void setOverhead(final short overhead) {
        this.mOverhead = overhead;
    }

    public float getOverheadBackAltDiff() {
        return mOverheadBackAltDiff;
    }

    public void setOverheadBackAltDiff(final float overheadBackAltDiff) {
        this.mOverheadBackAltDiff = overheadBackAltDiff;
    }

    public int getFloor() {
        return mFloor;
    }

    public void setFloor(final int floor) {
        this.mFloor = floor;
    }

    public String getExtendInfoFlag() {
        return mExtendInfoFlag;
    }

    public void setExtendInfoFlag(final String extendInfoFlag) {
        this.mExtendInfoFlag = extendInfoFlag;
    }

    public String getSrcApp() {
        return mSrcApp;
    }

    public void setSrcApp(final String srcApp) {
        this.mSrcApp = srcApp;
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

    public String getRetainParam() {
        return mRetainParam;
    }

    public void setRetainParam(final String retainParam) {
        this.mRetainParam = retainParam;
    }

    public int getAdCode() {
        return mAdCode;
    }

    public void setAdCode(final int adCode) {
        this.mAdCode = adCode;
    }

    @Override
    public String toString() {
        return "RouteParam{" +
                "poiType=" + mPoiType +
                "realPos=" + mRealPos +
                ", naviPos=" + mNaviPos +
                ", type=" + mType +
                ", sigshelter=" + mSigshelter +
                ", roadID=" + mRoadID +
                ", poiID='" + mPoiID + '\'' +
                ", name='" + mName + '\'' +
                ", typeCode='" + mTypeCode + '\'' +
                ", fromJump=" + mFromJump +
                ", overhead=" + mOverhead +
                ", overheadBackAltDiff=" + mOverheadBackAltDiff +
                ", floor=" + mFloor +
                ", extendInfoFlag='" + mExtendInfoFlag + '\'' +
                ", srcApp='" + mSrcApp + '\'' +
                ", address='" + mAddress + '\'' +
                ", chargeInfo=" + mChargeInfo +
                ", retainParam='" + mRetainParam + '\'' +
                ", adCode='" + mAdCode + '\'' +
                '}';
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(final @NonNull Parcel parcel, final int i) {
        parcel.writeInt(mPoiType);
        parcel.writeParcelable(mRealPos, i);
        parcel.writeParcelable(mNaviPos, i);
        parcel.writeInt(mType);
        parcel.writeFloat(mSigshelter);
        parcel.writeLong(mRoadID);
        parcel.writeString(mPoiID);
        parcel.writeString(mName);
        parcel.writeString(mTypeCode);
        parcel.writeInt((int) mFromJump);
        parcel.writeInt((int) mOverhead);
        parcel.writeFloat(mOverheadBackAltDiff);
        parcel.writeInt(mFloor);
        parcel.writeString(mExtendInfoFlag);
        parcel.writeString(mSrcApp);
        parcel.writeString(mAddress);
        parcel.writeString(mRetainParam);
        parcel.writeInt(mAdCode);
        parcel.writeParcelable(mPoiInfoEntity, i);
    }
}

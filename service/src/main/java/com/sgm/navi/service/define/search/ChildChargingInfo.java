package com.sgm.navi.service.define.search;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import com.sgm.navi.service.define.bean.GeoPoint;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

@Data
@NoArgsConstructor
@Accessors(chain = true)
public class ChildChargingInfo implements Parcelable {
    //子点的父子关系类型
    private int mChildType;
    //价格
    private double mPrice;
    //简称
    private String mShortName;
    //名称
    private String mName;
    //唯一标识
    private String mPoiId;
    //父节点的唯一标识
    private String mParentPoiId;
    //类型编码
    private String mTypeCode;
    //慢充电桩数量
    private int mNumSlow;
    //快充电桩数量
    private int mNumFast;
    //坐标
    private GeoPoint mLocation;

    public int getChildType() {
        return mChildType;
    }

    /**
     * 设置子点类型
     * @param childType 子点类型
     * @return ChildChargingInfo
     */
    public ChildChargingInfo setChildType(final int childType) {
        this.mChildType = childType;
        return this;
    }

    public double getPrice() {
        return mPrice;
    }

    /**
     * 设置价格
     * @param price 价格
     * @return ChildChargingInfo
     */
    public ChildChargingInfo setPrice(final double price) {
        this.mPrice = price;
        return this;
    }

    public String getShortName() {
        return mShortName;
    }

    /**
     * 设置简称
     * @param shortName 简称
     * @return ChildChargingInfo
     */
    public ChildChargingInfo setShortName(final String shortName) {
        this.mShortName = shortName;
        return this;
    }

    public String getName() {
        return mName;
    }

    /**
     * 设置名称
     * @param name 名称
     * @return ChildChargingInfo
     */
    public ChildChargingInfo setName(final String name) {
        this.mName = name;
        return this;
    }

    public String getPoiId() {
        return mPoiId;
    }

    /**
     * 设置唯一标识
     * @param poiId 唯一标识
     * @return ChildChargingInfo
     */
    public ChildChargingInfo setPoiId(final String poiId) {
        this.mPoiId = poiId;
        return this;
    }

    public String getParentPoiId() {
        return mParentPoiId;
    }

    /**
     * 设置父节点的唯一标识
     * @param parentPoiId 父节点的唯一标识
     * @return ChildChargingInfo
     */
    public ChildChargingInfo setParentPoiId(final String parentPoiId) {
        this.mParentPoiId = parentPoiId;
        return this;
    }

    public String getTypeCode() {
        return mTypeCode;
    }

    /**
     * 设置类型编码
     * @param typeCode 类型编码
     * @return ChildChargingInfo
     */
    public ChildChargingInfo setTypeCode(final String typeCode) {
        this.mTypeCode = typeCode;
        return this;
    }

    public int getNumSlow() {
        return mNumSlow;
    }

    /**
     * 设置慢速充电桩数量
     * @param numSlow 慢速充电桩数量
     * @return ChildChargingInfo
     */
    public ChildChargingInfo setNumSlow(final int numSlow) {
        this.mNumSlow = numSlow;
        return this;
    }

    public int getNumFast() {
        return mNumFast;
    }

    /**
     * 设置快速充电桩数量
     * @param numFast 快速充电桩数量
     * @return ChildChargingInfo
     */
    public ChildChargingInfo setNumFast(final int numFast) {
        this.mNumFast = numFast;
        return this;
    }

    public GeoPoint getLocation() {
        return mLocation;
    }

    /**
     * 设置位置
     * @param location 位置经纬度
     * @return ChildChargingInfo
     */
    public ChildChargingInfo setLocation(final GeoPoint location) {
        this.mLocation = location;
        return this;
    }

    protected ChildChargingInfo(final Parcel in) {
        mChildType = in.readInt();
        mPrice = in.readDouble();
        mShortName = in.readString();
        mName = in.readString();
        mPoiId = in.readString();
        mParentPoiId = in.readString();
        mTypeCode = in.readString();
        mNumSlow = in.readInt();
        mNumFast = in.readInt();
        mLocation = in.readParcelable(GeoPoint.class.getClassLoader());
    }

    public static final Creator<ChildChargingInfo> CREATOR = new Creator<ChildChargingInfo>() {
        @Override
        public ChildChargingInfo createFromParcel(final Parcel in) {
            return new ChildChargingInfo(in);
        }

        @Override
        public ChildChargingInfo[] newArray(final int size) {
            return new ChildChargingInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull final Parcel parcel, final int i) {
        parcel.writeInt(mChildType);
        parcel.writeDouble(mPrice);
        parcel.writeString(mShortName);
        parcel.writeString(mName);
        parcel.writeString(mPoiId);
        parcel.writeString(mParentPoiId);
        parcel.writeString(mTypeCode);
        parcel.writeInt(mNumSlow);
        parcel.writeInt(mNumFast);
        parcel.writeParcelable(mLocation, i);
    }
}

package com.fy.navi.service.define.search;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import com.fy.navi.service.define.bean.GeoPoint;

import java.util.ArrayList;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

@Data
@NoArgsConstructor
@Accessors(chain = true)
public class ChildInfo implements Parcelable {
    //唯一标识ID
    private String mPoiId;
    //名称
    private String mName;
    //简称
    private String mShortName;
    //子类型
    private int mChildType;
    //地址
    private String mAddress;
    //标签
    private String mLabel;
    //POI 的位置信息，包含经纬度。
    private GeoPoint mLocation;
    //POI 的入口位置信息，包含经纬度。
    private GeoPoint mPointEnter;
    //门点导航信息，用于行车场景下的导航标签细分。
    private int mNavigation;
    //子点推荐比例，单位为百分比。
    private double mRatio;
    //是否被选中
    private int mChecked;
    //子点充电站信息列表,当POI是景区或者商场且子点是停车场时有该信息
    private ArrayList<ChildChargingInfo> mChargingStationList;

    public String getPoiId() {
        return mPoiId;
    }

    /**
     * 设置poiId
     * @param poiId poiId
     * @return ChildInfo
     */
    public ChildInfo setPoiId(final String poiId) {
        this.mPoiId = poiId;
        return this;
    }

    public String getName() {
        return mName;
    }

    /**
     * 设置name
     * @param name name
     * @return ChildInfo
     */
    public ChildInfo setName(final String name) {
        this.mName = name;
        return this;
    }

    public String getShortName() {
        return mShortName;
    }

    /**
     * 设置简称
     * @param shortName 简称
     * @return ChildInfo
     */
    public ChildInfo setShortName(final String shortName) {
        this.mShortName = shortName;
        return this;
    }

    public int getChildType() {
        return mChildType;
    }

    /**
     * 设置子类型
     * @param childType 子类型
     * @return ChildInfo
     */
    public ChildInfo setChildType(final int childType) {
        this.mChildType = childType;
        return this;
    }

    public String getAddress() {
        return mAddress;
    }

    /**
     * 设置地址
     * @param address 地址
     * @return ChildInfo
     */
    public ChildInfo setAddress(final String address) {
        this.mAddress = address;
        return this;
    }

    public String getLabel() {
        return mLabel;
    }

    /**
     * 设置标签
     * @param label 标签
     * @return ChildInfo
     */
    public ChildInfo setLabel(final String label) {
        this.mLabel = label;
        return this;
    }

    public GeoPoint getLocation() {
        return mLocation;
    }

    /**
     * 设置位置
     * @param location 位置
     * @return ChildInfo
     */
    public ChildInfo setLocation(final GeoPoint location) {
        this.mLocation = location;
        return this;
    }

    public GeoPoint getPointEnter() {
        return mPointEnter;
    }

    /**
     * 设置入口经纬度
     * @param pointEnter 入口经纬度
     * @return ChildInfo
     */
    public ChildInfo setPointEnter(final GeoPoint pointEnter) {
        this.mPointEnter = pointEnter;
        return this;
    }

    public int getNavigation() {
        return mNavigation;
    }

    /**
     * 设置导航类型
     * @param navigation 导航类型
     * @return ChildInfo
     */
    public ChildInfo setNavigation(final int navigation) {
        this.mNavigation = navigation;
        return this;
    }

    public int getChecked() {
        return mChecked;
    }

    /**
     * 设置是否被选中
     * @param checked 是否被选中
     * @return ChildInfo
     */
    public ChildInfo setChecked(final int checked) {
        this.mChecked = checked;
        return this;
    }

    public double getRatio() {
        return mRatio;
    }

    /**
     * 设置比例
     * @param ratio 比例
     * @return ChildInfo
     */
    public ChildInfo setRatio(final double ratio) {
        this.mRatio = ratio;
        return this;
    }

    public ArrayList<ChildChargingInfo> getChargingStationList() {
        return mChargingStationList;
    }

    /**
     * 设置充电站列表
     * @param chargingStationList 充电站列表
     * @return ChildInfo
     */
    public ChildInfo setChargingStationList(final ArrayList<ChildChargingInfo> chargingStationList) {
        this.mChargingStationList = chargingStationList;
        return this;
    }

    protected ChildInfo(final Parcel in) {
        mPoiId = in.readString();
        mName = in.readString();
        mShortName = in.readString();
        mChildType = in.readInt();
        mAddress = in.readString();
        mLabel = in.readString();
        mLocation = in.readParcelable(GeoPoint.class.getClassLoader());
        mPointEnter = in.readParcelable(GeoPoint.class.getClassLoader());
        mNavigation = in.readInt();
        mRatio = in.readDouble();
        mChecked = in.readInt();
        mChargingStationList = in.createTypedArrayList(ChildChargingInfo.CREATOR);
    }

    public static final Creator<ChildInfo> CREATOR = new Creator<ChildInfo>() {
        @Override
        public ChildInfo createFromParcel(final Parcel in) {
            return new ChildInfo(in);
        }

        @Override
        public ChildInfo[] newArray(final int size) {
            return new ChildInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull final Parcel parcel, final int i) {
        parcel.writeString(mPoiId);
        parcel.writeString(mName);
        parcel.writeString(mShortName);
        parcel.writeInt(mChildType);
        parcel.writeString(mAddress);
        parcel.writeString(mLabel);
        parcel.writeParcelable(mLocation, i);
        parcel.writeParcelable(mPointEnter, i);
        parcel.writeInt(mNavigation);
        parcel.writeDouble(mRatio);
        parcel.writeInt(mChecked);
        parcel.writeTypedList(mChargingStationList);
    }
}

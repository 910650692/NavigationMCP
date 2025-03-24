package com.fy.navi.service.define.search;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import java.util.List;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 */

@Data
@NoArgsConstructor
@Accessors(chain = true)
public class ParkingInfo implements Parcelable {
    private int mSpaceTotal;
    private int mSpaceFree;
    private int mBusyStatus;
    private String mSrcType;
    private int mSpace;
    private String mFee;
    private String mGeometry;
    private String mCharge;
    private String mDayCharge;
    private String mNightCharge;
    private String mCategory;
    private String mParkingSrcType;
    private int mQueryType;
    private String mPoiId;
    private String mName;
    private String mTypeCode;
    private String mAddress = "";
    private List<SearchParkInOutInfo> mSearchParkInOutInfos;

    public int getSpaceFree() {
        return mSpaceFree;
    }

    /**
     * 设置空闲停车位
     * @param spaceFree 空闲停车位
     * @return ParkingInfo
     */
    public ParkingInfo setSpaceFree(final int spaceFree) {
        this.mSpaceFree = spaceFree;
        return this;
    }

    public int getSpaceTotal() {
        return mSpaceTotal;
    }

    /**
     * 设置总停车位
     * @param spaceTotal 总停车位
     * @return ParkingInfo
     */
    public ParkingInfo setSpaceTotal(final int spaceTotal) {
        this.mSpaceTotal = spaceTotal;
        return this;
    }

    public int getBusyStatus() {
        return mBusyStatus;
    }

    /**
     * 设置忙闲状态
     * @param busyStatus 忙闲状态
     * @return ParkingInfo
     */
    public ParkingInfo setBusyStatus(final int busyStatus) {
        this.mBusyStatus = busyStatus;
        return this;
    }

    public String getSrcType() {
        return mSrcType;
    }

    /**
     * 设置源类型
     * @param srcType 源类型
     * @return ParkingInfo
     */
    public ParkingInfo setSrcType(final String srcType) {
        this.mSrcType = srcType;
        return this;
    }

    public int getSpace() {
        return mSpace;
    }

    /**
     * 设置停车位
     * @param space 停车位
     * @return ParkingInfo
     */
    public ParkingInfo setSpace(final int space) {
        this.mSpace = space;
        return this;
    }

    public String getFee() {
        return mFee;
    }

    /**
     * 设置费用
     * @param fee 费用
     * @return ParkingInfo
     */
    public ParkingInfo setFee(final String fee) {
        this.mFee = fee;
        return this;
    }

    public String getGeometry() {
        return mGeometry;
    }

    /**
     * 设置坐标
     * @param geometry 坐标
     * @return ParkingInfo
     */
    public ParkingInfo setGeometry(final String geometry) {
        this.mGeometry = geometry;
        return this;
    }

    public String getCharge() {
        return mCharge;
    }

    /**
     * 设置收费
     * @param charge 收费
     * @return ParkingInfo
     */
    public ParkingInfo setCharge(final String charge) {
        this.mCharge = charge;
        return this;
    }

    public String getDayCharge() {
        return mDayCharge;
    }

    /**
     * 设置白天收费
     * @param dayCharge 白天收费
     * @return ParkingInfo
     */
    public ParkingInfo setDayCharge(final String dayCharge) {
        this.mDayCharge = dayCharge;
        return this;
    }

    public String getNightCharge() {
        return mNightCharge;
    }

    /**
     * 设置夜间收费
     * @param nightCharge 夜间收费
     * @return ParkingInfo
     */
    public ParkingInfo setNightCharge(final String nightCharge) {
        this.mNightCharge = nightCharge;
        return this;
    }

    public String getCategory() {
        return mCategory;
    }

    /**
     * 设置类别
     * @param category 类别
     * @return ParkingInfo
     */
    public ParkingInfo setCategory(final String category) {
        this.mCategory = category;
        return this;
    }

    public String getParkingSrcType() {
        return mParkingSrcType;
    }

    /**
     * 设置停车源类型
     * @param parkingSrcType 停车源类型
     * @return ParkingInfo
     */
    public ParkingInfo setParkingSrcType(final String parkingSrcType) {
        this.mParkingSrcType = parkingSrcType;
        return this;
    }

    public int getQueryType() {
        return mQueryType;
    }

    /**
     * 设置查询类型
     * @param queryType 查询类型
     * @return ParkingInfo
     */
    public ParkingInfo setQueryType(final int queryType) {
        this.mQueryType = queryType;
        return this;
    }

    public String getPoiId() {
        return mPoiId;
    }

    /**
     * 设置poiId
     * @param poiId poiId
     * @return ParkingInfo
     */
    public ParkingInfo setPoiId(final String poiId) {
        this.mPoiId = poiId;
        return this;
    }

    public String getName() {
        return mName;
    }

    /**
     * 设置名称
     * @param name 名称
     * @return ParkingInfo
     */
    public ParkingInfo setName(final String name) {
        this.mName = name;
        return this;
    }

    public String getTypeCode() {
        return mTypeCode;
    }

    /**
     * 设置类型编码
     * @param typeCode 类型编码
     * @return ParkingInfo
     */
    public ParkingInfo setTypeCode(final String typeCode) {
        this.mTypeCode = typeCode;
        return this;
    }

    public String getAddress() {
        return mAddress;
    }

    /**
     * 设置地址
     * @param address 地址
     * @return ParkingInfo
     */
    public ParkingInfo setAddress(final String address) {
        this.mAddress = address;
        return this;
    }

    public List<SearchParkInOutInfo> getSearchParkInOutInfos() {
        return mSearchParkInOutInfos;
    }

    /**
     * 设置查询停车进出信息
     * @param searchParkInOutInfos 查询停车进出信息
     * @return ParkingInfo
     */
    public ParkingInfo setSearchParkInOutInfos(final List<SearchParkInOutInfo> searchParkInOutInfos) {
        this.mSearchParkInOutInfos = searchParkInOutInfos;
        return this;
    }

    protected ParkingInfo(final Parcel in) {
        mSpaceTotal = in.readInt();
        mSpaceFree = in.readInt();
        mBusyStatus = in.readInt();
        mSrcType = in.readString();
        mSpace = in.readInt();
        mFee = in.readString();
        mGeometry = in.readString();
        mCharge = in.readString();
        mDayCharge = in.readString();
        mNightCharge = in.readString();
        mCategory = in.readString();
        mParkingSrcType = in.readString();
        mQueryType = in.readInt();
        mPoiId = in.readString();
        mName = in.readString();
        mTypeCode = in.readString();
        mAddress = in.readString();
        mSearchParkInOutInfos = in.createTypedArrayList(SearchParkInOutInfo.CREATOR);
    }

    public static final Creator<ParkingInfo> CREATOR = new Creator<ParkingInfo>() {
        @Override
        public ParkingInfo createFromParcel(final Parcel in) {
            return new ParkingInfo(in);
        }

        @Override
        public ParkingInfo[] newArray(final int size) {
            return new ParkingInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull final Parcel parcel, final int i) {
        parcel.writeInt(mSpaceTotal);
        parcel.writeInt(mSpaceFree);
        parcel.writeInt(mBusyStatus);
        parcel.writeString(mSrcType);
        parcel.writeInt(mSpace);
        parcel.writeString(mFee);
        parcel.writeString(mGeometry);
        parcel.writeString(mCharge);
        parcel.writeString(mDayCharge);
        parcel.writeString(mNightCharge);
        parcel.writeString(mCategory);
        parcel.writeString(mParkingSrcType);
        parcel.writeInt(mQueryType);
        parcel.writeString(mPoiId);
        parcel.writeString(mName);
        parcel.writeString(mTypeCode);
        parcel.writeString(mAddress);
        parcel.writeTypedList(mSearchParkInOutInfos);
    }
}

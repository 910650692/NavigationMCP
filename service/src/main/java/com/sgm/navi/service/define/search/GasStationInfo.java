package com.sgm.navi.service.define.search;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

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
public class GasStationInfo implements Parcelable {
    private int mQueryType;
    private String mPoiId;
    private String mName;
    private String mTypeCode;
    private String mType;
    private String mPrice;
    private boolean mDiscount = false;

    public int getQueryType() {
        return mQueryType;
    }

    /**
     * 设置查询类型
     * @param queryType 查询类型
     * @return GasStationInfo
     */
    public GasStationInfo setQueryType(final int queryType) {
        this.mQueryType = queryType;
        return this;
    }

    public String getPoiId() {
        return mPoiId;
    }

    /**
     * 设置poiId
     * @param poiId poiId
     * @return GasStationInfo
     */
    public GasStationInfo setPoiId(final String poiId) {
        this.mPoiId = poiId;
        return this;
    }

    public String getName() {
        return mName;
    }

    /**
     * 设置名称
     * @param name 名称
     * @return GasStationInfo
     */
    public GasStationInfo setName(final String name) {
        this.mName = name;
        return this;
    }

    public String getType() {
        return mType;
    }

    /**
     * 设置类型
     * @param type 类型
     * @return GasStationInfo
     */
    public GasStationInfo setType(final String type) {
        this.mType = type;
        return this;
    }

    public String getTypeCode() {
        return mTypeCode;
    }

    /**
     * 设置类型代码
     * @param typeCode 类型代码
     * @return GasStationInfo
     */
    public GasStationInfo setTypeCode(final String typeCode) {
        this.mTypeCode = typeCode;
        return this;
    }

    public String getPrice() {
        return mPrice;
    }

    /**
     * 设置价格
     * @param price 价格
     * @return GasStationInfo
     */
    public GasStationInfo setPrice(final String price) {
        this.mPrice = price;
        return this;
    }

    public boolean isDiscount() {
        return mDiscount;
    }

    /**
     * 设置是否打折
     * @param discount 是否打折
     * @return GasStationInfo
     */
    public GasStationInfo setDiscount(final boolean discount) {
        this.mDiscount = discount;
        return this;
    }

    protected GasStationInfo(final Parcel in) {
        mQueryType = in.readInt();
        mPoiId = in.readString();
        mName = in.readString();
        mTypeCode = in.readString();
        mType = in.readString();
        mPrice = in.readString();
        mDiscount = in.readByte() != 0;
    }

    public static final Creator<GasStationInfo> CREATOR = new Creator<GasStationInfo>() {
        @Override
        public GasStationInfo createFromParcel(final Parcel in) {
            return new GasStationInfo(in);
        }

        @Override
        public GasStationInfo[] newArray(final int size) {
            return new GasStationInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull final Parcel parcel, final int i) {
        parcel.writeInt(mQueryType);
        parcel.writeString(mPoiId);
        parcel.writeString(mName);
        parcel.writeString(mTypeCode);
        parcel.writeString(mType);
        parcel.writeString(mPrice);
        parcel.writeByte((byte) (mDiscount ? 1 : 0));
    }
}

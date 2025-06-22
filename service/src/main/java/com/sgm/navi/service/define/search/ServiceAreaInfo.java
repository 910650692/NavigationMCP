package com.sgm.navi.service.define.search;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import com.sgm.navi.service.define.bean.GeoPoint;

import java.util.List;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: ServiceAreaInfo
 * @CreateDate: 2025/2/12 15:21
 */

@Data
@NoArgsConstructor
@Accessors(chain = true)
public class ServiceAreaInfo implements Parcelable {
    private int mQueryType;
    private String mPoiId;
    private String mName;
    private String mTypeCode;
    private int mBuilding = 0;
    private String mServiceStar = "";
    private String mBrand = "";
    private String mAddress = "";
    private List<ServiceAreaChild> mServiceAreaChildList;

    public int getQueryType() {
        return mQueryType;
    }

    /**
     * 设置查询类型
     * @param queryType 查询类型
     * @return ServiceAreaInfo
     */
    public ServiceAreaInfo setQueryType(final int queryType) {
        this.mQueryType = queryType;
        return this;
    }

    public String getPoiId() {
        return mPoiId;
    }

    /**
     * 设置poiId
     * @param poiId poiId
     * @return ServiceAreaInfo
     */
    public ServiceAreaInfo setPoiId(final String poiId) {
        this.mPoiId = poiId;
        return this;
    }

    public String getName() {
        return mName;
    }

    /**
     * 设置名称
     * @param name 名称
     * @return ServiceAreaInfo
     */
    public ServiceAreaInfo setName(final String name) {
        this.mName = name;
        return this;
    }

    public String getTypeCode() {
        return mTypeCode;
    }

    /**
     * 设置类型编码
     * @param typeCode 类型编码
     * @return ServiceAreaInfo
     */
    public ServiceAreaInfo setTypeCode(final String typeCode) {
        this.mTypeCode = typeCode;
        return this;
    }

    public int getBuilding() {
        return mBuilding;
    }

    /**
     * 设置楼层
     * @param building 楼层
     * @return ServiceAreaInfo
     */
    public ServiceAreaInfo setBuilding(final int building) {
        this.mBuilding = building;
        return this;
    }

    public String getServiceStar() {
        return mServiceStar;
    }

    /**
     * 设置服务星级
     * @param serviceStar 服务星级
     * @return ServiceAreaInfo
     */
    public ServiceAreaInfo setServiceStar(final String serviceStar) {
        this.mServiceStar = serviceStar;
        return this;
    }

    public String getBrand() {
        return mBrand;
    }

    /**
     * 设置品牌
     * @param brand 品牌
     * @return ServiceAreaInfo
     */
    public ServiceAreaInfo setBrand(final String brand) {
        this.mBrand = brand;
        return this;
    }

    public String getAddress() {
        return mAddress;
    }

    /**
     * 设置地址
     * @param address 地址
     * @return ServiceAreaInfo
     */
    public ServiceAreaInfo setAddress(final String address) {
        this.mAddress = address;
        return this;
    }

    public List<ServiceAreaChild> getServiceAreaChildList() {
        return mServiceAreaChildList;
    }

    /**
     * 设置子区域列表
     * @param serviceAreaChildList 子区域列表
     * @return ServiceAreaInfo
     */
    public ServiceAreaInfo setServiceAreaChildList(final List<ServiceAreaChild> serviceAreaChildList) {
        this.mServiceAreaChildList = serviceAreaChildList;
        return this;
    }

    protected ServiceAreaInfo(final Parcel in) {
        mQueryType = in.readInt();
        mPoiId = in.readString();
        mName = in.readString();
        mTypeCode = in.readString();
        mBuilding = in.readInt();
        mServiceStar = in.readString();
        mBrand = in.readString();
        mAddress = in.readString();
        mServiceAreaChildList = in.createTypedArrayList(ServiceAreaChild.CREATOR);
    }

    public static final Creator<ServiceAreaInfo> CREATOR = new Creator<ServiceAreaInfo>() {
        @Override
        public ServiceAreaInfo createFromParcel(final Parcel in) {
            return new ServiceAreaInfo(in);
        }

        @Override
        public ServiceAreaInfo[] newArray(final int size) {
            return new ServiceAreaInfo[size];
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
        parcel.writeInt(mBuilding);
        parcel.writeString(mServiceStar);
        parcel.writeString(mBrand);
        parcel.writeString(mAddress);
        parcel.writeTypedList(mServiceAreaChildList);
    }


    @Data
    @NoArgsConstructor
    @Accessors(chain = true)
    public static class ServiceAreaChild implements Parcelable {
        private boolean mDiscount;
        private String mGasType;
        private String mId;
        private String mName;
        private String minMame;
        private String mParentId;
        private String mTypeCode;
        private String mBusiness;
        private String mTag;
        private GeoPoint mPoiLoc;

        public boolean isDiscount() {
            return mDiscount;
        }

        /**
         * 设置折扣
         * @param discount 折扣
         * @return ServiceAreaChild
         */
        public ServiceAreaChild setDiscount(final boolean discount) {
            this.mDiscount = discount;
            return this;
        }

        public String getGasType() {
            return mGasType;
        }

        /**
         * 设置油品类型
         * @param gasType 油品类型
         * @return ServiceAreaChild
         */
        public ServiceAreaChild setGasType(final String gasType) {
            this.mGasType = gasType;
            return this;
        }

        public String getId() {
            return mId;
        }

        /**
         * 设置id
         * @param id id
         * @return ServiceAreaChild
         */
        public ServiceAreaChild setId(final String id) {
            this.mId = id;
            return this;
        }

        public String getName() {
            return mName;
        }

        /**
         * 设置名称
         * @param name 名称
         * @return ServiceAreaChild
         */
        public ServiceAreaChild setName(final String name) {
            this.mName = name;
            return this;
        }

        public String getMinMame() {
            return minMame;
        }

        /**
         * 设置最小名称
         * @param minMame   最小名称
         * @return ServiceAreaChild
         */
        public ServiceAreaChild setMinMame(final String minMame) {
            this.minMame = minMame;
            return this;
        }

        public String getParentId() {
            return mParentId;
        }

        /**
         * 设置父id
         * @param parentId 父id
         * @return ServiceAreaChild
         */
        public ServiceAreaChild setParentId(final String parentId) {
            this.mParentId = parentId;
            return this;
        }

        public String getTypeCode() {
            return mTypeCode;
        }

        /**
         * 设置类型编码
         * @param typeCode  类型编码
         * @return ServiceAreaChild
         */
        public ServiceAreaChild setTypeCode(final String typeCode) {
            this.mTypeCode = typeCode;
            return this;
        }

        public String getBusiness() {
            return mBusiness;
        }

        /**
         * 设置营业时间
         * @param business 营业时间
         * @return ServiceAreaChild
         */
        public ServiceAreaChild setBusiness(final String business) {
            this.mBusiness = business;
            return this;
        }

        public String getTag() {
            return mTag;
        }

        /**
         * 设置标签
         * @param tag 标签
         * @return ServiceAreaChild
         */
        public ServiceAreaChild setTag(final String tag) {
            this.mTag = tag;
            return this;
        }

        public GeoPoint getPoi_loc() {
            return mPoiLoc;
        }

        /**
         * 设置poi的位置
         * @param poiLoc poi的位置
         * @return ServiceAreaChild
         */
        public ServiceAreaChild setPoi_loc(final GeoPoint poiLoc) {
            this.mPoiLoc = poiLoc;
            return this;
        }

        protected ServiceAreaChild(final Parcel in) {
            mDiscount = in.readByte() != 0;
            mGasType = in.readString();
            mId = in.readString();
            mName = in.readString();
            minMame = in.readString();
            mParentId = in.readString();
            mTypeCode = in.readString();
            mBusiness = in.readString();
            mTag = in.readString();
        }

        public static final Creator<ServiceAreaChild> CREATOR = new Creator<ServiceAreaChild>() {
            @Override
            public ServiceAreaChild createFromParcel(final Parcel in) {
                return new ServiceAreaChild(in);
            }

            @Override
            public ServiceAreaChild[] newArray(final int size) {
                return new ServiceAreaChild[size];
            }
        };

        @Override
        public int describeContents() {
            return 0;
        }

        @Override
        public void writeToParcel(@NonNull final Parcel parcel, final int i) {
            parcel.writeByte((byte) (mDiscount ? 1 : 0));
            parcel.writeString(mGasType);
            parcel.writeString(mId);
            parcel.writeString(mName);
            parcel.writeString(minMame);
            parcel.writeString(mParentId);
            parcel.writeString(mTypeCode);
            parcel.writeString(mBusiness);
            parcel.writeString(mTag);
        }
    }
}

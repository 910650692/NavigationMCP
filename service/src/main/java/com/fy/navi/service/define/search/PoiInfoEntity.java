package com.fy.navi.service.define.search;


import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import com.android.utils.ConvertUtils;
import com.fy.navi.service.define.bean.GeoPoint;

import java.util.ArrayList;
import java.util.List;

import lombok.Data;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: 使用 Lombok 自动生成样板代码：
 * •@Data：自动生成 Getter、Setter、toString、equals 和 hashCode。
 * •@NoArgsConstructor：生成无参构造方法。
 * •@Accessors(chain = true)：支持链式调用，例如 entity.setName("POI").setCityCode("123");。
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
public class PoiInfoEntity implements Parcelable {
    private int mPoiType;           // poi 类型
    private String mPid;            // 父POI的Id
    private String mName;           // 名称
    private String mPhone;          // 电话
    private String mAddress;        // 地址
    private String mCategory;       // 分类，筛选使用
    private int mAdCode;            // 市区县详细代码
    private int mCityCode;          // 省市代码
    private String mDistance;       // 距离，默认带单位
    private String mTypeCode;       // POI搜索类型
    private String mPoiTag;         // POI标签
    private String mShortName;      // 简称
    private String mBusinessTime;    // 营业时间
    private double mRatio;          // 选择率
    private String mOpenStatus;     // 营业状态
    private int mHisMark;           // 个性化标签（美食、洗车）
    private String mScenicMark;     // 个性化标签（景区）
    private int mAverageCost;       // 人均消费
    private String mRating;         // 星级评分
    private String mImageUrl;       // 图片信息
    private GeoPoint mPoint;        // 经纬度
    private String mPointTypeCode;  // POI类型（扎标专用）
    private String mIndustry;       // 行业类型

    // -----------语音排序使用------------
    private int mSortDistance;     // 距离，语音使用
    private int mSortRate;        // 评价，语音使用
    private int mSortPrice;      // 价格，语音使用

    //-----------
    private CityInfo mCityInfo;
    // 收藏相关
    private FavoriteInfo mFavoriteInfo;
    //回传参数，POI详情搜索必传，来自于关键字搜索结果
    private SearchRetainParamInfo mRetainParam;
    // 服务列表区信息
    private List<ServiceAreaInfo> mServiceAreaInfoList;
    // 停车场列表信息
    private List<ParkingInfo> mParkingInfoList;
    // 充电列表站信息
    private List<ChargeInfo> mChargeInfoList;
    // 加油站列表信息
    private List<GasStationInfo> mStationList;
    // 子POI列表信息
    private List<ChildInfo> mChildInfoList;
    // 区域边界点列表
    private ArrayList<ArrayList<GeoPoint>> mPoiAoiBounds;
    // 道路边界点列表
    private ArrayList<ArrayList<GeoPoint>> mRoadPolygonBounds;

    private List<ChargePriceInfo> mChargePriceInfoList;

    private List<ChargeEquipmentInfo> mChargeEquipmentInfoList;

    public int getPoiType() {
        return mPoiType;
    }

    public boolean getIsEndPoint() {
        return mIsEndPoint;
    }

    public void setIsEndPoint(boolean isEndPoint) {
        this.mIsEndPoint = isEndPoint;
    }

    // 是否是终点
    private boolean mIsEndPoint = false;

    /**
     * 设置poi类型
     * @param poiType poi类型
     * @return PoiInfoEntity
     */
    public PoiInfoEntity setPoiType(final int poiType) {
        this.mPoiType = poiType;
        return this;
    }

    public String getPid() {
        return mPid;
    }

    /**
     * 设置poi的id
     * @param pid poi的id
     * @return PoiInfoEntity
     */
    public PoiInfoEntity setPid(final String pid) {
        this.mPid = pid;
        return this;
    }

    public String getName() {
        return mName;
    }

    /**
     * 设置poi的名称
     * @param name poi的名称
     * @return PoiInfoEntity
     */
    public PoiInfoEntity setName(final String name) {
        this.mName = name;
        return this;
    }

    public String getPhone() {
        return mPhone;
    }

    /**
     * 设置poi的电话
     * @param phone poi的电话
     * @return PoiInfoEntity
     */
    public PoiInfoEntity setPhone(final String phone) {
        this.mPhone = phone;
        return this;
    }

    public String getAddress() {
        return mAddress;
    }

    /**
     * 设置poi的地址
     * @param address poi的地址
     * @return PoiInfoEntity
     */
    public PoiInfoEntity setAddress(final String address) {
        this.mAddress = address;
        return this;
    }

    public String getCategory() {
        return mCategory;
    }

    /**
     * 设置poi的类别
     * @param category poi的类别
     * @return PoiInfoEntity
     */
    public PoiInfoEntity setCategory(final String category) {
        this.mCategory = category;
        return this;
    }

    public int getAdCode() {
        return mAdCode;
    }

    /**
     * 设置poi的adcode
     * @param adCode poi的adcode
     * @return PoiInfoEntity
     */
    public PoiInfoEntity setAdCode(final int adCode) {
        this.mAdCode = adCode;
        return this;
    }

    public String getDistance() {
        return mDistance;
    }

    /**
     * 设置poi的距离
     * @param distance poi的距离
     * @return PoiInfoEntity
     */
    public PoiInfoEntity setDistance(final String distance) {
        this.mDistance = distance;
        return this;
    }

    public String getTypeCode() {
        return mTypeCode;
    }

    /**
     * 设置poi的typeCode
     * @param typeCode poi的typeCode
     * @return PoiInfoEntity
     */
    public PoiInfoEntity setTypeCode(final String typeCode) {
        this.mTypeCode = typeCode;
        return this;
    }

    public String getPoiTag() {
        return mPoiTag;
    }

    /**
     * 设置poi的tag
     * @param poiTag poi的tag
     * @return PoiInfoEntity
     */
    public PoiInfoEntity setPoiTag(final String poiTag) {
        this.mPoiTag = poiTag;
        return this;
    }

    public String getShortName() {
        return mShortName;
    }

    /**
     * 设置poi的shortName
     * @param shortName poi的shortName
     * @return PoiInfoEntity
     */
    public PoiInfoEntity setShortName(final String shortName) {
        this.mShortName = shortName;
        return this;
    }

    public String getBusinessTime() {
        return mBusinessTime;
    }

    /**
     * 设置poi的营业时间
     * @param businessTime  poi的营业时间
     * @return PoiInfoEntity
     */
    public PoiInfoEntity setBusinessTime(final String businessTime) {
        this.mBusinessTime = businessTime;
        return this;
    }

    public double getRatio() {
        return mRatio;
    }

    /**
     * 设置poi的选择率
     * @param ratio poi的选择率
     * @return PoiInfoEntity
     */
    public PoiInfoEntity setRatio(final double ratio) {
        this.mRatio = ratio;
        return this;
    }

    public String getOpenStatus() {
        return mOpenStatus;
    }

    /**
     * 设置poi的营业状态
     * @param openStatus poi的营业状态
     * @return PoiInfoEntity
     */
    public PoiInfoEntity setOpenStatus(final String openStatus) {
        this.mOpenStatus = openStatus;
        return this;
    }

    public String getScenicMark() {
        return mScenicMark;
    }

    /**
     * 设置个性化标签（景区）
     * @param scenicMark 个性化标签（景区）
     * @return PoiInfoEntity
     */
    public PoiInfoEntity setScenicMark(final String scenicMark) {
        this.mScenicMark = scenicMark;
        return this;
    }

    public int getHisMark() {
        return mHisMark;
    }

    /**
     * 设置历史标签
     * @param hisMark 历史标签
     * @return PoiInfoEntity
     */
    public PoiInfoEntity setHisMark(final int hisMark) {
        this.mHisMark = hisMark;
        return this;
    }

    public int getAverageCost() {
        return mAverageCost;
    }

    /**
     * 设置平均消费
     * @param averageCost 平均消费
     * @return PoiInfoEntity
     */
    public PoiInfoEntity setAverageCost(final int averageCost) {
        this.mAverageCost = averageCost;
        return this;
    }

    public String getImageUrl() {
        return mImageUrl;
    }

    /**
     * 设置poi的图片url
     * @param imageUrl poi的图片url
     * @return PoiInfoEntity
     */
    public PoiInfoEntity setImageUrl(final String imageUrl) {
        this.mImageUrl = imageUrl;
        return this;
    }

    public String getRating() {
        return mRating;
    }

    /**
     * 设置poi的评分
     * @param rating poi的评分
     * @return PoiInfoEntity
     */
    public PoiInfoEntity setRating(final String rating) {
        this.mRating = rating;
        return this;
    }

    public GeoPoint getPoint() {
        return mPoint;
    }

    /**
     * 设置poi的地理位置
     * @param point poi的地理位置
     * @return PoiInfoEntity
     */
    public PoiInfoEntity setPoint(final GeoPoint point) {
        this.mPoint = point;
        return this;
    }

    public String getPointTypeCode() {
        return mPointTypeCode;
    }

    /**
     * 设置poi的pointTypeCode
     * @param pointTypeCode poi的pointTypeCode
     * @return PoiInfoEntity
     */
    public PoiInfoEntity setPointTypeCode(final String pointTypeCode) {
        this.mPointTypeCode = pointTypeCode;
        return this;
    }

    public String getIndustry() {
        return mIndustry;
    }

    /**
     * 设置POI的行业类型
     * @param industry POI的行业类型
     * @return PoiInfoEntity
     */
    public PoiInfoEntity setIndustry(final String industry) {
        this.mIndustry = industry;
        return this;
    }

    public int getSort_distance() {
        return mSortDistance;
    }

    /**
     * 设置排序距离
     * @param sortDistance 排序距离
     * @return PoiInfoEntity
     */
    public PoiInfoEntity setSort_distance(final int sortDistance) {
        this.mSortDistance = sortDistance;
        return this;
    }

    public int getSort_rate() {
        return mSortRate;
    }

    /**
     * 设置评价
     * @param sortRate 评价
     * @return PoiInfoEntity
     */
    public PoiInfoEntity setSort_rate(final int sortRate) {
        this.mSortRate = sortRate;
        return this;
    }

    public int getSort_price() {
        return mSortPrice;
    }

    /**
     * 设置价格
     * @param sortPrice 价格
     * @return PoiInfoEntity
     */
    public PoiInfoEntity setSort_price(final int sortPrice) {
        this.mSortPrice = sortPrice;
        return this;
    }

    public CityInfo getCityInfo() {
        return mCityInfo;
    }

    /**
     * 设置城市信息
     * @param cityInfo 城市信息
     * @return PoiInfoEntity
     */
    public PoiInfoEntity setCityInfo(final CityInfo cityInfo) {
        this.mCityInfo = cityInfo;
        return this;
    }

    public FavoriteInfo getFavoriteInfo() {
        return mFavoriteInfo;
    }

    /**
     * 设置收藏信息
     * @param favoriteInfo 收藏信息
     * @return PoiInfoEntity
     */
    public PoiInfoEntity setFavoriteInfo(final FavoriteInfo favoriteInfo) {
        this.mFavoriteInfo = favoriteInfo;
        return this;
    }

    public SearchRetainParamInfo getRetainParam() {
        return mRetainParam;
    }

    /**
     * 设置回传参数，POI详情搜索必传，来自于关键字搜索结果
     * @param retainParam 回传参数，POI详情搜索必传，来自于关键字搜索结果
     * @return PoiInfoEntity
     */
    public PoiInfoEntity setRetainParam(final SearchRetainParamInfo retainParam) {
        this.mRetainParam = retainParam;
        return this;
    }

    public List<ChargeInfo> getChargeInfoList() {
        return mChargeInfoList;
    }

    /**
     * 设置充电站信息列表
     * @param chargeInfoList 充电站信息列表
     * @return PoiInfoEntity
     */
    public PoiInfoEntity setChargeInfoList(final List<ChargeInfo> chargeInfoList) {
        this.mChargeInfoList = chargeInfoList;
        return this;
    }

    public List<ServiceAreaInfo> getServiceAreaInfoList() {
        return mServiceAreaInfoList;
    }

    /**
     * 设置服务区域信息列表
     * @param serviceAreaInfoList 服务区域信息列表
     * @return PoiInfoEntity
     */
    public PoiInfoEntity setServiceAreaInfoList(final List<ServiceAreaInfo> serviceAreaInfoList) {
        this.mServiceAreaInfoList = serviceAreaInfoList;
        return this;
    }

    public List<ParkingInfo> getParkingInfoList() {
        return mParkingInfoList;
    }

    /**
     * 设置停车信息列表
     * @param parkingInfoList 停车信息列表
     * @return PoiInfoEntity
     */
    public PoiInfoEntity setParkingInfoList(final List<ParkingInfo> parkingInfoList) {
        this.mParkingInfoList = parkingInfoList;
        return this;
    }

    public List<GasStationInfo> getStationList() {
        return mStationList;
    }

    /**
     * 设置加油站信息列表
     * @param stationList 加油站信息列表
     * @return PoiInfoEntity
     */
    public PoiInfoEntity setStationList(final List<GasStationInfo> stationList) {
        this.mStationList = stationList;
        return this;
    }

    public List<ChildInfo> getChildInfoList() {
        return mChildInfoList;
    }

    /**
     * 设置子节点信息列表
     * @param childInfoList 子节点信息列表
     * @return PoiInfoEntity
     */
    public PoiInfoEntity setChildInfoList(final List<ChildInfo> childInfoList) {
        this.mChildInfoList = childInfoList;
        return this;
    }

    public PoiInfoEntity setmChargePriceInfoList(List<ChargePriceInfo> mChargePriceInfoList) {
        this.mChargePriceInfoList = mChargePriceInfoList;
        return this;
    }

    public List<ChargePriceInfo> getmChargePriceInfoList() {
        return mChargePriceInfoList;
    }

    public List<ChargeEquipmentInfo> getmChargeEquipmentInfoList() {
        return mChargeEquipmentInfoList;
    }

    public PoiInfoEntity setmChargeEquipmentInfoList(List<ChargeEquipmentInfo> mChargeEquipmentInfoList) {
        this.mChargeEquipmentInfoList = mChargeEquipmentInfoList;
        return this;
    }

    protected PoiInfoEntity(final Parcel in) {
        mPoiType = in.readInt();
        mPid = in.readString();
        mName = in.readString();
        mPhone = in.readString();
        mAddress = in.readString();
        mCategory = in.readString();
        mAdCode = in.readInt();
        mCityCode = in.readInt();
        mDistance = in.readString();
        mTypeCode = in.readString();
        mPoiTag = in.readString();
        mShortName = in.readString();
        mBusinessTime = in.readString();
        mRatio = in.readDouble();
        mOpenStatus = in.readString();
        mHisMark = in.readInt();
        mScenicMark = in.readString();
        mAverageCost = in.readInt();
        mRating = in.readString();
        mImageUrl = in.readString();
        mPoint = in.readParcelable(GeoPoint.class.getClassLoader());
        mPointTypeCode = in.readString();
        mIndustry = in.readString();
        mSortDistance = in.readInt();
        mSortRate = in.readInt();
        mSortPrice = in.readInt();
        mCityInfo = in.readParcelable(CityInfo.class.getClassLoader());
        mFavoriteInfo = in.readParcelable(FavoriteInfo.class.getClassLoader());
        mRetainParam = in.readParcelable(SearchRetainParamInfo.class.getClassLoader());
        mServiceAreaInfoList = in.createTypedArrayList(ServiceAreaInfo.CREATOR);
        mParkingInfoList = in.createTypedArrayList(ParkingInfo.CREATOR);
        mChargeInfoList = in.createTypedArrayList(ChargeInfo.CREATOR);
        mStationList = in.createTypedArrayList(GasStationInfo.CREATOR);
        mChildInfoList = in.createTypedArrayList(ChildInfo.CREATOR);
        mChargePriceInfoList = in.createTypedArrayList(ChargePriceInfo.CREATOR);
        mChargeEquipmentInfoList = in.createTypedArrayList(ChargeEquipmentInfo.CREATOR);
        // 新增字段 mPoiAoiBounds 的反序列化
        // 读取外层列表的大小
        final int outerSize = in.readInt();
        mPoiAoiBounds = new ArrayList<>();
        for (int i = 0; i < outerSize; i++) {
            // 创建内层列表
            final ArrayList<GeoPoint> innerList = new ArrayList<>();
            // 读取内层列表的 GeoPoint 对象
            in.readTypedList(innerList, GeoPoint.CREATOR);
            mPoiAoiBounds.add(innerList);
        }

        // 新增字段 mRoadPolygonBounds 的反序列化
        // 读取外层列表的大小
        final int outerRoadSize = in.readInt();
        mRoadPolygonBounds = new ArrayList<>();
        for (int i = 0; i < outerRoadSize; i++) {
            // 创建内层列表
            final ArrayList<GeoPoint> innerList = new ArrayList<>();
            // 读取内层列表的 GeoPoint 对象
            in.readTypedList(innerList, GeoPoint.CREATOR);
            mRoadPolygonBounds.add(innerList);
        }
    }

    public static final Creator<PoiInfoEntity> CREATOR = new Creator<PoiInfoEntity>() {
        @Override
        public PoiInfoEntity createFromParcel(final Parcel in) {
            return new PoiInfoEntity(in);
        }

        @Override
        public PoiInfoEntity[] newArray(final int size) {
            return new PoiInfoEntity[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull final Parcel parcel, final int i) {
        parcel.writeInt(mPoiType);
        parcel.writeString(mPid);
        parcel.writeString(mName);
        parcel.writeString(mPhone);
        parcel.writeString(mAddress);
        parcel.writeString(mCategory);
        parcel.writeInt(mAdCode);
        parcel.writeInt(mCityCode);
        parcel.writeString(mDistance);
        parcel.writeString(mTypeCode);
        parcel.writeString(mPoiTag);
        parcel.writeString(mShortName);
        parcel.writeString(mBusinessTime);
        parcel.writeDouble(mRatio);
        parcel.writeString(mOpenStatus);
        parcel.writeInt(mHisMark);
        parcel.writeString(mScenicMark);
        parcel.writeInt(mAverageCost);
        parcel.writeString(mRating);
        parcel.writeString(mImageUrl);
        parcel.writeParcelable(mPoint, i);
        parcel.writeString(mPointTypeCode);
        parcel.writeString(mIndustry);
        parcel.writeInt(mSortDistance);
        parcel.writeInt(mSortRate);
        parcel.writeInt(mSortPrice);
        parcel.writeParcelable(mCityInfo, i);
        parcel.writeParcelable(mFavoriteInfo, i);
        parcel.writeParcelable(mRetainParam, i);
        parcel.writeTypedList(mServiceAreaInfoList);
        parcel.writeTypedList(mParkingInfoList);
        parcel.writeTypedList(mChargeInfoList);
        parcel.writeTypedList(mStationList);
        parcel.writeTypedList(mChildInfoList);
        parcel.writeTypedList(mChargePriceInfoList);
        parcel.writeTypedList(mChargeEquipmentInfoList);
        if(ConvertUtils.isEmpty(mPoiAoiBounds)) {
            return;
        }
        parcel.writeInt(mPoiAoiBounds.size());
        // 遍历每个内层列表
        for (ArrayList<GeoPoint> innerList : mPoiAoiBounds) {
            // 写入内层列表的大小
            parcel.writeInt(innerList.size());
            // 写入内层列表的 ChildInfo 对象
            parcel.writeTypedList(innerList);
        }

        if(ConvertUtils.isEmpty(mRoadPolygonBounds)) {
            return;
        }
        parcel.writeInt(mRoadPolygonBounds.size());
        // 遍历每个内层列表
        for (ArrayList<GeoPoint> innerList : mRoadPolygonBounds) {
            // 写入内层列表的大小
            parcel.writeInt(innerList.size());
            // 写入内层列表的 ChildInfo 对象
            parcel.writeTypedList(innerList);
        }
    }
}
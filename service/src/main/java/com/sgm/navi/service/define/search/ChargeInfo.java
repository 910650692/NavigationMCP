package com.sgm.navi.service.define.search;

import android.annotation.SuppressLint;
import android.os.Parcel;
import android.os.Parcelable;
import android.text.TextUtils;

import androidx.annotation.NonNull;

import com.android.utils.log.Logger;
import com.google.gson.annotations.SerializedName;

import java.util.ArrayList;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * @version \$Revision1.0\$
 * @author baipeng0904
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
public class ChargeInfo implements Parcelable {
    private int mChildType = 0;
    private int mSlowFree = 0;
    private int mSlowTotal = 0;
    private int mFastFree = 0;
    private int mFastTotal = 0;
    private int mSuperFree = -1;
    private int mSuperTotal = -1;
    private String mSrcType = "";
    private String market = "";
    private String mParkCategory = "";
    private String mOpenTime = "";
    private boolean mOpen24h = false;
    private float mFMax;
    private float mFMin;
    private String mCurrentElePrice;
    private String mCurrentServicePrice;
    private int maxPower;
    private int minPower;
    private int mQueryType;
    private String mPoiId;
    private String mName;
    private String mTypeCode;
    private int mSlowVolt;
    private int mSlowPower;
    private int mFastVolt;
    private int mFastPower;
    private boolean mIsAppointment;  //是否预约
    private long mLatestChargeTimestamp; // 表示最近一次充电的时间戳（单位为秒）
    private long mSearchTimestamp;
    /** SGM 自营站字段 **/
    private String mOperatorId; // 运营商id
    private String mStationId; // 充电站id
    private String mBrand; // 品牌
    @SerializedName("costItem")
    private ArrayList<CostTime> mCostItem;
    @SerializedName("equipmentInfoItem")
    private ArrayList<EquipmentInfo> mEquipmentInfo;
    @SerializedName("slowChargingFree")
    private int mSlowChargingFree;
    @SerializedName("slowChargingTotal")
    private int mSlowChargingTotal;
    @SerializedName("fastChargingFree")
    private int mFastChargingFree;
    @SerializedName("fastChargingTotal")
    private int mFastChargingTotal;
    @SerializedName("parkFee")
    private String mParkFee;
    @SerializedName("lowPrice")
    private String mLowPrice;

    public int getChildType() {
        return mChildType;
    }

    /**
     * 设置子项类型
     * @param childType 子项类型
     * @return ChargeInfo
     */
    public ChargeInfo setChildType(final int childType) {
        this.mChildType = childType;
        return this;
    }

    public int getSlow_free() {
        return mSlowFree;
    }

    /**
     * 设置慢充桩空闲数量
     * @param slowFree 慢充桩空闲数量
     * @return ChargeInfo
     */
    public ChargeInfo setSlow_free(final int slowFree) {
        this.mSlowFree = slowFree;
        return this;
    }

    public int getSlow_total() {
        return mSlowTotal;
    }

    /**
     * 设置慢充桩总数量
     * @param slowTotal 慢充桩总数量
     * @return ChargeInfo
     */
    public ChargeInfo setSlow_total(final int slowTotal) {
        this.mSlowTotal = slowTotal;
        return this;
    }

    public int getFast_free() {
        return mFastFree;
    }

    /**
     * 设置快充桩空闲数量
     * @param fastFree 慢充桩空闲数量
     * @return ChargeInfo
     */
    public ChargeInfo setFast_free(final int fastFree) {
        this.mFastFree = fastFree;
        return this;
    }

    public int getFast_total() {
        return mFastTotal;
    }

    /**
     * 设置快充桩总数量
     * @param fastTotal 快充桩总数量
     * @return ChargeInfo
     */
    public ChargeInfo setFast_total(final int fastTotal) {
        this.mFastTotal = fastTotal;
        return this;
    }

    public int getSuperFree() {
        return mSuperFree;
    }

    /**
     * 设置超充桩空闲数量
     * @param superFree 超充桩空闲数量
     * @return ChargeInfo
     */
    public ChargeInfo setSuperFree(final int superFree) {
        this.mSuperFree = superFree;
        return this;
    }

    public int getSuperTotal() {
        return mSuperTotal;
    }

    /**
     * 设置超充桩总数量
     * @param superTotal 超充桩总数量
     * @return ChargeInfo
     */
    public ChargeInfo setSuperTotal(final int superTotal) {
        this.mSuperTotal = superTotal;
        return this;
    }

    public String getSrcType() {
        return mSrcType;
    }

    /**
     * 设置srcType
     * @param srcType srcType
     * @return ChargeInfo
     */
    public ChargeInfo setSrcType(final String srcType) {
        this.mSrcType = srcType;
        return this;
    }

    public String getMarket() {
        return market;
    }

    /**
     * 设置market
     * @param market market
     * @return ChargeInfo
     */
    public ChargeInfo setMarket(final String market) {
        this.market = market;
        return this;
    }

    public String getParkCategory() {
        return mParkCategory;
    }

    /**
     * 设置停车场类型
     * @param parkCategory 停车场类型
     * @return ChargeInfo
     */
    public ChargeInfo setParkCategory(final String parkCategory) {
        this.mParkCategory = parkCategory;
        return this;
    }

    public String getOpenTime() {
        return mOpenTime;
    }

    /**
     * 设置开始营业时间
     * @param openTime 开始营业时间
     * @return ChargeInfo
     */
    public ChargeInfo setOpenTime(final String openTime) {
        this.mOpenTime = openTime;
        return this;
    }

    public boolean isOpen24h() {
        return mOpen24h;
    }

    /**
     * 设置是否24h营业
     * @param open24h 是否24h营业
     * @return ChargeInfo
     */
    public ChargeInfo setOpen24h(final boolean open24h) {
        this.mOpen24h = open24h;
        return this;
    }

    /**
     * 最高电压
     * @return float
     */
    public float getfMax() {
        return mFMax;
    }

    /**
     * 最高电压
     * @param max float
     * @return ChargeInfo
     */
    public ChargeInfo setfMax(final float max) {
        this.mFMax = max;
        return this;
    }

    /**
     * 最低电压
     * @return float
     */
    public float getfMin() {
        return mFMin;
    }

    /**
     * 最低电压
     * @param min float
     * @return ChargeInfo
     */
    public ChargeInfo setfMin(final float min) {
        this.mFMin = min;
        return this;
    }

    public String getCurrentElePrice() {
        return mCurrentElePrice;
    }

    /**
     * 设置当前电价
     * @param currentElePrice 当前电价
     * @return ChargeInfo
     */
    @SuppressLint("DefaultLocale")
    public ChargeInfo setCurrentElePrice(final String currentElePrice) {
        String price = currentElePrice;
        if (TextUtils.isEmpty(currentElePrice)) {
            price = "--";
        } else {
            try {
                price = String.format("%.2f", Double.parseDouble(currentElePrice));
            } catch (Exception e) {
                Logger.e("ERROR", "setCurrentElePrice error, msg = " + e.getMessage());
            }
        }
        this.mCurrentElePrice = price;
        return this;
    }

    public String getCurrentServicePrice() {
        return mCurrentServicePrice;
    }

    /**
     * 设置当前活动价格
     * @param currentServicePrice 当前活动价格
     * @return ChargeInfo
     */
    public ChargeInfo setCurrentServicePrice(final String currentServicePrice) {
        this.mCurrentServicePrice = currentServicePrice;
        return this;
    }

    public int getMaxPower() {
        return maxPower;
    }

    /**
     * 设置最大功率
     * @param maxPower 最大功率
     * @return ChargeInfo
     */
    public ChargeInfo setMaxPower(final int maxPower) {
        this.maxPower = maxPower;
        return this;
    }

    public int getQueryType() {
        return mQueryType;
    }

    /**
     * 设置搜索类别
     * @param queryType 搜索类别
     * @return ChargeInfo
     */
    public ChargeInfo setQueryType(final int queryType) {
        this.mQueryType = queryType;
        return this;
    }

    public int getMinPower() {
        return minPower;
    }

    /**
     * 设置最小功率
     * @param minPower 最小功率
     * @return ChargeInfo
     */
    public ChargeInfo setMinPower(final int minPower) {
        this.minPower = minPower;
        return this;
    }

    public String getPoiId() {
        return mPoiId;
    }

    /**
     * 设置poiId
     * @param poiId poiId
     * @return ChargeInfo
     */
    public ChargeInfo setPoiId(final String poiId) {
        this.mPoiId = poiId;
        return this;
    }

    public String getName() {
        return mName;
    }

    /**
     * 设置名称
     * @param name 名称
     * @return ChargeInfo
     */
    public ChargeInfo setName(final String name) {
        this.mName = name;
        return this;
    }

    public String getTypeCode() {
        return mTypeCode;
    }

    /**
     * 设置类别编码
     * @param typeCode 类别编码
     * @return ChargeInfo
     */
    public ChargeInfo setTypeCode(final String typeCode) {
        this.mTypeCode = typeCode;
        return this;
    }

    public int getSlowVolt() {
        return mSlowVolt;
    }

    /**
     * 设置慢充电电压
     * @param slowVolt 慢充电电压
     * @return ChargeInfo
     */
    public ChargeInfo setSlowVolt(final int slowVolt) {
        this.mSlowVolt = slowVolt;
        return this;
    }

    public int getSlowPower() {
        return mSlowPower;
    }

    /**
     * 设置慢充电功率
     * @param slowPower 慢充电功率
     * @return ChargeInfo
     */
    public ChargeInfo setSlowPower(final int slowPower) {
        this.mSlowPower = slowPower;
        return this;
    }

    public int getFastVolt() {
        return mFastVolt;
    }

    /**
     * 设置快充电电压
     * @param fastVolt 快充电电压
     * @return ChargeInfo
     */
    public ChargeInfo setFastVolt(final int fastVolt) {
        this.mFastVolt = fastVolt;
        return this;
    }

    public int getFastPower() {
        return mFastPower;
    }

    /**
     * 设置快充电功率
     * @param fastPower 快充电功率
     * @return ChargeInfo
     */
    public ChargeInfo setFastPower(final int fastPower) {
        this.mFastPower = fastPower;
        return this;
    }

    public String getOperatorId() {
        return mOperatorId;
    }

    /**
     * 运营商id
     * @param operatorId 充电站运营时间
     * @return PoiInfoEntity
     */
    public ChargeInfo setOperatorId(String operatorId) {
        this.mOperatorId = operatorId;
        return this;
    }

    public String getStationId() {
        return mStationId;
    }

    /**
     * 充电站id
     * @param stationId 充电站id
     * @return PoiInfoEntity
     */
    public ChargeInfo setStationId(String stationId) {
        this.mStationId = stationId;
        return this;
    }

    public ArrayList<CostTime> getCostItem() {
        return mCostItem;
    }

    /**
     * 获取全时段费用列表
     * @param mCostItem 全时段费用列表
     * @return PoiInfoEntity
     */
    public ChargeInfo setCostItem(ArrayList<CostTime> mCostItem) {
        this.mCostItem = mCostItem;
        return this;
    }

    public ArrayList<EquipmentInfo> getEquipmentInfo() {
        return mEquipmentInfo;
    }

    /**
     * 充电桩信息
     * @param mEquipmentInfo 全时段费用列表
     * @return PoiInfoEntity
     */
    public ChargeInfo setEquipmentInfo(ArrayList<EquipmentInfo> mEquipmentInfo) {
        this.mEquipmentInfo = mEquipmentInfo;
        return this;
    }

    public int getSlowChargingFree() {
        return mSlowChargingFree;
    }

    /**
     * 剩余慢充
     * @param slowChargingFree 剩余慢充
     * @return PoiInfoEntity
     */
    public ChargeInfo setSlowChargingFree(int slowChargingFree) {
        this.mSlowChargingFree = slowChargingFree;
        return this;
    }

    public int getSlowChargingTotal() {
        return mSlowChargingTotal;
    }

    /**
     * 慢充数量
     * @param slowChargingTotal 慢充数量
     * @return PoiInfoEntity
     */
    public ChargeInfo setSlowChargingTotal(int slowChargingTotal) {
        this.mSlowChargingTotal = slowChargingTotal;
        return this;
    }

    public int getFastChargingFree() {
        return mFastChargingFree;
    }

    /**
     * 剩余快充
     * @param fastChargingFree 剩余快充
     * @return PoiInfoEntity
     */
    public ChargeInfo setFastChargingFree(int fastChargingFree) {
        this.mFastChargingFree = fastChargingFree;
        return this;
    }
    public int getFastChargingTotal() {
        return mFastChargingTotal;
    }

    /**
     * 快充数量
     * @param fastChargingTotal 快充数量
     * @return PoiInfoEntity
     */
    public ChargeInfo setFastChargingTotal(int fastChargingTotal) {
        this.mFastChargingTotal = fastChargingTotal;
        return this;
    }

    public String getParkFee() {
        return mParkFee;
    }

    /**
     * 停车场价格
     * @param parkFee 停车场价格
     * @return PoiInfoEntity
     */
    public ChargeInfo setParkFee(String parkFee) {
        this.mParkFee = parkFee;
        return this;
    }

    public String getLowPrice() {
        return mLowPrice;
    }

    /**
     * 充电价格
     * @param lowPrice 充电价格
     * @return PoiInfoEntity
     */
    public void setLowPrice(String lowPrice) {
        this.mLowPrice = lowPrice;
    }
    protected ChargeInfo(final Parcel in) {
        mChildType = in.readInt();
        mSlowFree = in.readInt();
        mSlowTotal = in.readInt();
        mFastFree = in.readInt();
        mFastTotal = in.readInt();
        mSuperFree = in.readInt();
        mSuperTotal = in.readInt();
        mSrcType = in.readString();
        market = in.readString();
        mParkCategory = in.readString();
        mOpenTime = in.readString();
        mOpen24h = in.readByte() != 0;
        mFMax = in.readFloat();
        mFMin = in.readFloat();
        mCurrentElePrice = in.readString();
        mCurrentServicePrice = in.readString();
        maxPower = in.readInt();
        minPower = in.readInt();
        mQueryType = in.readInt();
        mPoiId = in.readString();
        mName = in.readString();
        mTypeCode = in.readString();
        mSlowVolt = in.readInt();
        mSlowPower = in.readInt();
        mFastVolt = in.readInt();
        mFastPower = in.readInt();
        mOperatorId = in.readString();
        mStationId = in.readString();
        mBrand = in.readString();
        mSlowChargingFree = in.readInt();
        mSlowChargingTotal = in.readInt();
        mFastChargingFree = in.readInt();
        mFastChargingTotal = in.readInt();
        mParkFee = in.readString();
        mLowPrice = in.readString();
        mCostItem = in.createTypedArrayList(CostTime.CREATOR);
        mEquipmentInfo = in.createTypedArrayList(EquipmentInfo.CREATOR);
        mIsAppointment = in.readBoolean();
        mLatestChargeTimestamp = in.readLong();
        mSearchTimestamp = in.readLong();
    }

    public static final Creator<ChargeInfo> CREATOR = new Creator<ChargeInfo>() {
        @Override
        public ChargeInfo createFromParcel(final Parcel in) {
            return new ChargeInfo(in);
        }

        @Override
        public ChargeInfo[] newArray(final int size) {
            return new ChargeInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull final Parcel parcel, final int i) {
        parcel.writeInt(mChildType);
        parcel.writeInt(mSlowFree);
        parcel.writeInt(mSlowTotal);
        parcel.writeInt(mFastFree);
        parcel.writeInt(mFastTotal);
        parcel.writeInt(mSuperFree);
        parcel.writeInt(mSuperTotal);
        parcel.writeString(mSrcType);
        parcel.writeString(market);
        parcel.writeString(mParkCategory);
        parcel.writeString(mOpenTime);
        parcel.writeByte((byte) (mOpen24h ? 1 : 0));
        parcel.writeFloat(mFMax);
        parcel.writeFloat(mFMin);
        parcel.writeString(mCurrentElePrice);
        parcel.writeString(mCurrentServicePrice);
        parcel.writeInt(maxPower);
        parcel.writeInt(minPower);
        parcel.writeInt(mQueryType);
        parcel.writeString(mPoiId);
        parcel.writeString(mName);
        parcel.writeString(mTypeCode);
        parcel.writeInt(mSlowVolt);
        parcel.writeInt(mSlowPower);
        parcel.writeInt(mFastVolt);
        parcel.writeInt(mFastPower);
        parcel.writeString(mOperatorId);
        parcel.writeString(mStationId);
        parcel.writeString(mBrand);
        parcel.writeTypedList(mCostItem);
        parcel.writeTypedList(mEquipmentInfo);
        parcel.writeBoolean(mIsAppointment);
        parcel.writeInt(mSlowChargingFree);
        parcel.writeInt(mSlowChargingTotal);
        parcel.writeInt(mFastChargingFree);
        parcel.writeInt(mFastChargingTotal);
        parcel.writeString(mParkFee);
        parcel.writeString(mLowPrice);
        parcel.writeLong(mLatestChargeTimestamp);
        parcel.writeLong(mSearchTimestamp);
    }
}

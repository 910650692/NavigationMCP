package com.sgm.navi.service.define.mapdata;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import java.util.ArrayList;

import lombok.NoArgsConstructor;

@NoArgsConstructor
public class ProvDataInfo implements Parcelable {
    // 行政编码
    private int mAdCode;
    // 行政区域类型
    private int mAreaType;
    // 区域名称，如福建省、厦门市
    private String mName;
    // 区域拼音简拼，福建省（fjs）
    private String mJianPin;
    // 区域拼音全拼，福建省（fujiansheng）
    private String mPinYin;
    // 上级行政区域adcode
    private int mUpperAdcode;
    // 同级附近5个行政区adcode列表
    private ArrayList<Integer> mVecNearAdcodeList;
    // 下级行政区Id列表
    private ArrayList<Integer> mVecLowerAdcodeList;
    // 省辖市列表
    private ArrayList<CityDataInfo> mCityInfoList = new ArrayList<>();
    // 模糊搜索 - 一级城市数据包对应的下载状态信息
    private CityDownLoadInfo mDownLoadInfo;
    private boolean mIsExpanded;

    protected ProvDataInfo(Parcel in) {
        mAdCode = in.readInt();
        mAreaType = in.readInt();
        mName = in.readString();
        mJianPin = in.readString();
        mPinYin = in.readString();
        mUpperAdcode = in.readInt();
        mIsExpanded = in.readByte() != 0;
    }

    public static final Creator<ProvDataInfo> CREATOR = new Creator<ProvDataInfo>() {
        @Override
        public ProvDataInfo createFromParcel(Parcel in) {
            return new ProvDataInfo(in);
        }

        @Override
        public ProvDataInfo[] newArray(int size) {
            return new ProvDataInfo[size];
        }
    };

    public boolean isExpanded() {
        return mIsExpanded;
    }

    public void setExpanded(boolean expanded) {
        mIsExpanded = expanded;
    }

    public int getAdcode() {
        return mAdCode;
    }

    public void setAdcode(final int adcode) {
        this.mAdCode = adcode;
    }

    public int getAreaType() {
        return mAreaType;
    }

    public void setAreaType(final int areaType) {
        this.mAreaType = areaType;
    }

    public String getName() {
        return mName;
    }

    public void setName(final String name) {
        this.mName = name;
    }

    public String getJianPin() {
        return mJianPin;
    }

    public void setJianPin(final String jianPin) {
        this.mJianPin = jianPin;
    }

    public String getPinYin() {
        return mPinYin;
    }

    public void setPinYin(final String pinYin) {
        this.mPinYin = pinYin;
    }

    public int getUpperAdcode() {
        return mUpperAdcode;
    }

    public void setUpperAdcode(final int upperAdcode) {
        this.mUpperAdcode = upperAdcode;
    }

    public ArrayList<Integer> getVecNearAdcodeList() {
        return mVecNearAdcodeList;
    }

    public void setVecNearAdcodeList(final ArrayList<Integer> vecNearAdcodeList) {
        this.mVecNearAdcodeList = vecNearAdcodeList;
    }

    public ArrayList<Integer> getVecLowerAdcodeList() {
        return mVecLowerAdcodeList;
    }

    public void setVecLowerAdcodeList(final ArrayList<Integer> vecLowerAdcodeList) {
        this.mVecLowerAdcodeList = vecLowerAdcodeList;
    }

    public ArrayList<CityDataInfo> getCityInfoList() {
        return mCityInfoList;
    }

    public void setCityInfoList(final ArrayList<CityDataInfo> cityInfoList) {
        this.mCityInfoList = cityInfoList;
    }

    public CityDownLoadInfo getDownLoadInfo() {
        return mDownLoadInfo;
    }

    public void setDownLoadInfo(final CityDownLoadInfo downLoadInfo) {
        this.mDownLoadInfo = downLoadInfo;
    }

    /**
     * 判断搜索时是否是一个单独条目的城市
     * @return
     */
    public boolean isParentCity() {
        return mAreaType == 2 || mAreaType == 3 || mAreaType == 4;
    }


    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeInt(mAdCode);
        dest.writeInt(mAreaType);
        dest.writeString(mName);
        dest.writeString(mJianPin);
        dest.writeString(mPinYin);
        dest.writeInt(mUpperAdcode);
        dest.writeByte((byte) (mIsExpanded ? 1 : 0));
    }
}

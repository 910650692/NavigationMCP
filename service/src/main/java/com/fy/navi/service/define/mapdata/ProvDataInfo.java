package com.fy.navi.service.define.mapdata;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class ProvDataInfo implements Serializable {
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
    private List<CityDataInfo> mCityInfoList;
    // 模糊搜索 - 一级城市数据包对应的下载状态信息
    private CityDownLoadInfo mDownLoadInfo;

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

    public List<CityDataInfo> getCityInfoList() {
        return mCityInfoList;
    }

    public void setCityInfoList(final List<CityDataInfo> cityInfoList) {
        this.mCityInfoList = cityInfoList;
    }

    public CityDownLoadInfo getDownLoadInfo() {
        return mDownLoadInfo;
    }

    public void setDownLoadInfo(final CityDownLoadInfo downLoadInfo) {
        this.mDownLoadInfo = downLoadInfo;
    }
}

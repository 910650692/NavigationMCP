package com.fy.navi.service.define.mapdata;

import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;

public class CityDownLoadInfo {
    private int mAdCode;
    // 是否有效下载项
    private boolean mValidItem;
    // 下载任务执行的状态
    private int mTaskState;
    private String mStatusTip;
    // 操作错误码
    private int mErrCode;
    // 下载进度
    private float mPercent;
    // 是否有数据更新
    private boolean mUpdate;
    // 是否有在用数据
    private boolean mIsDataUsed;
    // 是否发现完整高版本数据
    private boolean mIsCompletelyHighVer;
    //  全量zip包解压后大小，单位byte
    private BigInteger mFullUnpackSize;
    // 增量包解压后大小，单位Byte
    private BigInteger mUnpackSize;
    // 全量zip包大小，单位byte
    private BigInteger mFullZipSize;
    // 增量zip包大小，单位Byte
    private BigInteger mZipSize;
    //同一个省份下所有城市的下载状态
    private Map<Integer, Integer> mAllCityTaskStateMap;


    /**
     * getAllCityTaskStateMap
     * @return map
     */
    public Map<Integer, Integer> getAllCityTaskStateMap() {
        return mAllCityTaskStateMap;
    }

    /**
     * setAllCityTaskStateMap
     * @param allCityTaskStateMap
     */
    public void setAllCityTaskStateMap(Map<Integer, Integer> allCityTaskStateMap) {
        mAllCityTaskStateMap = allCityTaskStateMap;
    }

    /**
     * 获取参数
     * @return 返回 int 数据
     */
    public int getAdcode() {
        return mAdCode;
    }

    public void setAdcode(final int adcode) {
        this.mAdCode = adcode;
    }

    /**
     * 获取参数
     * @return 返回boolean 数据
     */
    public boolean isValidItem() {
        return mValidItem;
    }

    /**
     * * 设置参数
     * @param validItem
     */
    public void setValidItem(final boolean validItem) {
        this.mValidItem = validItem;
    }

    /**
     * 获取参数
     * @return 返回 int 数据
     */
    public int getTaskState() {
        return mTaskState;
    }

    /**
     * * 设置参数
     * @param taskState
     */
    public void setTaskState(final int taskState) {
        this.mTaskState = taskState;
    }

    /**
     * 获取参数
     * @return 返回 String 数据
     */
    public String getStatusTip() {
        return mStatusTip;
    }

    /**
     * * 设置参数
     * @param statusTip
     */
    public void setStatusTip(final String statusTip) {
        this.mStatusTip = statusTip;
    }

    /**
     * 获取参数
     * @return 返回 int 数据
     */
    public int getErrCode() {
        return mErrCode;
    }

    /**
     * * 设置参数
     * @param errCode
     */
    public void setErrCode(final int errCode) {
        this.mErrCode = errCode;
    }

    /**
     * 获取参数
     * @return 返回 float 数据
     */
    public float getPercent() {
        return mPercent;
    }

    /**
     * * 设置参数
     * @param percent
     */
    public void setPercent(final float percent) {
        this.mPercent = percent;
    }

    /**
     * 获取参数
     * @return 返回 boolean 数据
     */
    public boolean isUpdate() {
        return mUpdate;
    }

    /**
     * * 设置参数
     * @param update
     */
    public void setUpdate(final boolean update) {
        this.mUpdate = update;
    }

    /**
     * 获取参数
     * @return 返回 boolean 数据
     */
    public boolean isIsDataUsed() {
        return mIsDataUsed;
    }

    /**
     * * 设置参数
     * @param isDataUsed
     */
    public void setIsDataUsed(final boolean isDataUsed) {
        this.mIsDataUsed = isDataUsed;
    }

    /**
     * 获取参数
     * @return 返回 boolean 数据
     */
    public boolean isCompletelyHighVer() {
        return mIsCompletelyHighVer;
    }

    /**
     * * 设置参数
     * @param completelyHighVer
     */
    public void setCompletelyHighVer(final boolean completelyHighVer) {
        mIsCompletelyHighVer = completelyHighVer;
    }

    /**
     * 获取参数
     * @return 返回 BigInteger 数据
     */
    public BigInteger getFullUnpackSize() {
        return mFullUnpackSize;
    }

    /**
     * * 设置参数
     * @param fullUnpackSize
     */
    public void setFullUnpackSize(final BigInteger fullUnpackSize) {
        this.mFullUnpackSize = fullUnpackSize;
    }


    /**
     * 获取参数
     * @return 返回 BigInteger 数据
     */
    public BigInteger getUnpackSize() {
        return mUnpackSize;
    }

    /**
     * * 设置参数
     * @param unPackSize
     */
    public void setUnpackSize(final BigInteger unPackSize) {
        this.mUnpackSize = unPackSize;
    }


    /**
     * 获取参数
     * @return 返回 BigInteger 数据
     */
    public BigInteger getFullZipSize() {
        return mFullZipSize;
    }

    /**
     * * 设置参数
     * @param fullZipSize
     */
    public void setFullZipSize(final BigInteger fullZipSize) {
        this.mFullZipSize = fullZipSize;
    }


    /**
     * 获取参数
     * @return 返回 BigInteger 数据
     */
    public BigInteger getZipSize() {
        return mZipSize;
    }

    /**
     * * 设置参数
     * @param zipSize
     */
    public void setZipSize(final BigInteger zipSize) {
        this.mZipSize = zipSize;
    }
}

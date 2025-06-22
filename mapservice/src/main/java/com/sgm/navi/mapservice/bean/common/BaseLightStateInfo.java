package com.sgm.navi.mapservice.bean.common;

//红绿灯状态信息
public class BaseLightStateInfo {

    //灯态类型
    private int mLightType;
    //灯态开始UTC时间戳 @unit 秒 @range >=0
    private long mStime;
    //灯态结束UTC时间戳 @unit 秒 @range >=0
    private long mEtime;
    //持续时间
    private int mLastSeconds;

    public int getLightType() {
        return mLightType;
    }

    public void setLightType(final int lightType) {
        mLightType = lightType;
    }

    public long getSTime() {
        return mStime;
    }

    public void setSTime(final long stime) {
        this.mStime = stime;
    }

    public long getETime() {
        return mEtime;
    }

    public void setETime(final long etime) {
        mEtime = etime;
    }

    public int getLastSeconds() {
        return mLastSeconds;
    }

    public void setLastSeconds(final int lastSeconds) {
        mLastSeconds = lastSeconds;
    }

    @Override
    public String toString() {
        return "BaseLightStateInfo{" +
                "mLightType=" + mLightType +
                ", mStime=" + mStime +
                ", mEtime=" + mEtime +
                ", mLastSeconds=" + mLastSeconds +
                '}';
    }

}

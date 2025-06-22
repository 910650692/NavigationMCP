package com.sgm.navi.mapservice.bean.common;

import java.util.ArrayList;

//灯态信息
public class BaseLightInfo {

    //红绿灯指示方向 @unit 无 @range [ 0: 无效值 1: 左转 2: 右转 7: 调头 8: 直行 ]
    private int mDir;
    //等红绿灯轮数 @unit 轮 @range [ 0: 无需等待 >0: 等待轮数 ]
    //只有导航场景生效
    private long mWaitNum;
    //灯态描述，如：xx秒/红灯
    //只有导航场景生效
    private String mDesc;
    //显示形态标识 @unit 无 @range [ 0: 不展示灯态和倒计时 1: 仅展示倒计时 (实际不存在，因为有倒计时一定伴随有灯态) 2: 仅展示灯态，不展示倒计时 3: 展示灯态和倒计时 ]
    private int mShowType;
    //左转和直行是否同相位 @unit 无 @range [ 0: 非同相位，即灯态颜色或倒计时读秒不相同 1: 是同相位，即灯态颜色及倒计时读秒都相同 ]
    //只有巡航场景生效
    private int mPhase;
    //灯规格类型 @unit 无 @range [ 0: 无效值 1: 常规灯，有颜色及读秒变化的灯 2: 降级灯，仅有颜色变化的灯 ]
    private int mStandardType;
    //提供未来几百秒的灯态信息
    private ArrayList<BaseLightStateInfo> mLightStates;

    public int getDir() {
        return mDir;
    }

    public void setDir(final int dir) {
        mDir = dir;
    }

    public long getWaitNum() {
        return mWaitNum;
    }

    public void setWaitNum(final long waitNum) {
        mWaitNum = waitNum;
    }

    public String getDesc() {
        return mDesc;
    }

    public void setDesc(final String desc) {
        mDesc = desc;
    }

    public int getShowType() {
        return mShowType;
    }

    public void setShowType(final int showType) {
        mShowType = showType;
    }

    public int getPhase() {
        return mPhase;
    }

    public void setPhase(final int phase) {
        mPhase = phase;
    }

    public int getStandardType() {
        return mStandardType;
    }

    public void setStandardType(final int standardType) {
        mStandardType = standardType;
    }

    public ArrayList<BaseLightStateInfo> getLightStates() {
        return mLightStates;
    }

    public void setLightStates(final ArrayList<BaseLightStateInfo> lightStates) {
        mLightStates = lightStates;
    }

    @Override
    public String toString() {
        return "BaseLightInfo{" +
                "mDir=" + mDir +
                ", mWaitNum=" + mWaitNum +
                ", mDesc='" + mDesc + '\'' +
                ", mShowType=" + mShowType +
                ", mPhase=" + mPhase +
                ", mStandardType=" + mStandardType +
                ", mLightStates=" + mLightStates +
                '}';
    }

}

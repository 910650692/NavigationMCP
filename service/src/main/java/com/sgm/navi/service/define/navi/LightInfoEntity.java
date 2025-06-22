package com.sgm.navi.service.define.navi;

import androidx.annotation.NonNull;

import java.util.ArrayList;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class LightInfoEntity {
    //红绿灯指示方向 @unit 无 @range [ 0: 无效值 1: 左转 2: 右转 7: 调头 8: 直行 ]
    public int mDir;
    //提供未来几百秒的灯态信息
    public ArrayList<LightStateEntity> mLightStates;
    //等红绿灯轮数 @unit 轮 @range [ 0: 无需等待 >0: 等待轮数 ]
    //只有导航场景生效
    public long mWaitNum;
    //灯态描述，如：xx秒/红灯
    //只有导航场景生效
    public String mDesc;
    //显示形态标识 @unit 无 @range [ 0: 不展示灯态和倒计时 1: 仅展示倒计时 (实际不存在，因为有倒计时一定伴随有灯态) 2: 仅展示灯态，不展示倒计时 3: 展示灯态和倒计时 ]
    public int mShowType;
    //左转和直行是否同相位 @unit 无 @range [ 0: 非同相位，即灯态颜色或倒计时读秒不相同 1: 是同相位，即灯态颜色及倒计时读秒都相同 ]
    //只有巡航场景生效
    public int mPhase;
    //灯规格类型 @unit 无 @range [ 0: 无效值 1: 常规灯，有颜色及读秒变化的灯 2: 降级灯，仅有颜色变化的灯 ]
    public int mStandardType;

    @Getter
    @Setter
    public static class LightStateEntity {
        //灯态类型
        public int mLightType;
        //灯态开始UTC时间戳 @unit 秒 @range >=0
        public long mStime;
        //灯态结束UTC时间戳 @unit 秒 @range >=0
        public long mEtime;
        //持续时间
        public int mLastSeconds;

        @NonNull
        @Override
        public String toString() {
            return "LightStateEntity{" +
                    "lightType=" + mLightType +
                    ", stime=" + mStime +
                    ", etime=" + mEtime +
                    ", lastSeconds=" + mLastSeconds +
                    '}';
        }
    }

    @NonNull
    @Override
    public String toString() {
        return "LightInfoEntity{" +
                "dir=" + mDir +
                ", lightStates=" + mLightStates +
                ", waitNum=" + mWaitNum +
                ", desc='" + mDesc + '\'' +
                ", showType=" + mShowType +
                ", phase=" + mPhase +
                ", standardType=" + mStandardType +
                '}';
    }
}

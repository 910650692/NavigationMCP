package com.sgm.navi.service.define.route;

import java.util.List;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RouteLineInfo {
    private long mPathID;//路线ID
    private int mType;//类型
    private String mLength;//路线长度
    private String mLabel;//正向标签
    private String mReverseLabel;//负向标签
    private String mTravelTime;//到达时间
    private String mStaticTravelTime;//静态到达时间
    private String mTollCost;//路线总收费金额
    private String mTrafficLightCount;//红绿灯个数
    private String mNaviID;//导航标识
    private boolean mIsOnline;//是否在线算路
    private boolean mElecRouteBool = true;//是否充电算路
    private String mElecRouteLabel;//是否在线算路
    private long mDistance;//数字长度
    private long mTotalTime;//总时长，单位s
    private boolean mChargingStation = false;//是否存在补能充电站

    private List<RouteLineSegmentInfo> mRouteLineSegmentInfos;
    private boolean mCanBeArrive = true;
    private boolean mRestoration = false;

    private int mRemainPercent = -1;

}

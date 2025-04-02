package com.fy.navi.service.define.route;


import java.math.BigInteger;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RouteWeatherInfo {
    private Coord3DDouble mPosition;
    private int mWeatherType;
    private long mPathID;
    private int mWeatherID;
    private RouteWeatherID mRouteWeatherID;
    private int mCityID;
    private long mTimestamp;
    private String mCityName;
    private String mWeatherName;
    private String mText;
    private int mRank;
    private BigInteger mLinkId;
    private long mDistance;
    private long mEta;
    private String mPlanChannelId;
    private String mAlertLevelName;// 预警名称
}

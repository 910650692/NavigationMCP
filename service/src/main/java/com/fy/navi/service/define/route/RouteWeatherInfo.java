package com.fy.navi.service.define.route;


import java.math.BigInteger;

public class RouteWeatherInfo {

    public Coord3DDouble mPosition;
    public int mWeatherType;
    public long mPathID;
    public int mWeatherID;
    public int mCityID;
    public long mTimestamp;
    public String mCityName;
    public String mWeatherName;
    public String mText;
    public int mRank;
    public BigInteger mLinkId;
    public long mDistance;
    public long mEta;
    public String mPlanChannelId;

    public RouteWeatherInfo() {
        this.mPosition = new Coord3DDouble();
        this.mWeatherType = 0;
        this.mPathID = 0L;
        this.mWeatherID = 0;
        this.mCityID = 0;
        this.mTimestamp = 0L;
        this.mCityName = "";
        this.mWeatherName = "";
        this.mText = "";
        this.mRank = 0;
        this.mLinkId = new BigInteger("0");
        this.mDistance = 0L;
        this.mEta = 0L;
        this.mPlanChannelId = "";
    }

    public RouteWeatherInfo(Coord3DDouble mPositionLiteObj, int mWeatherTypeLiteObj, long mPathIDLiteObj, int mWeatherIDLiteObj, int mCityIDLiteObj, long mTimestampLiteObj, String mCityNameLiteObj, String mWeatherNameLiteObj, String mTextLiteObj, int mRankLiteObj, BigInteger mLinkIdLiteObj, long mDistanceLiteObj, long mEtaLiteObj, String mPlanChannelIdLiteObj) {
        this.mPosition = mPositionLiteObj;
        this.mWeatherType = mWeatherTypeLiteObj;
        this.mPathID = mPathIDLiteObj;
        this.mWeatherID = mWeatherIDLiteObj;
        this.mCityID = mCityIDLiteObj;
        this.mTimestamp = mTimestampLiteObj;
        this.mCityName = mCityNameLiteObj;
        this.mWeatherName = mWeatherNameLiteObj;
        this.mText = mTextLiteObj;
        this.mRank = mRankLiteObj;
        this.mLinkId = mLinkIdLiteObj;
        this.mDistance = mDistanceLiteObj;
        this.mEta = mEtaLiteObj;
        this.mPlanChannelId = mPlanChannelIdLiteObj;
    }
}

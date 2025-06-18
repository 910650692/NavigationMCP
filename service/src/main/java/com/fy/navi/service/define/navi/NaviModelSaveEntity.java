package com.fy.navi.service.define.navi;

import java.util.List;

public class NaviModelSaveEntity {
    private SpeedOverallEntity mSpeedOverallEntity;
    private SapaInfoEntity mSapaInfoEntity;
    private NaviEtaInfo mNaviEtaInfo;
    private long mCrossProgress;
    private CrossImageEntity mCrossImageEntity;
    private NaviTmcInfo mNaviTmcInfo;
    private NaviManeuverInfo mNaviManeuverInfo;
    private LaneInfoEntity mLaneInfo;
    private List<NaviViaEntity> mViaList;
    private boolean mIsViaDetailShow;
    private NaviViaEntity mNaviViaEntity;
    private int mSapaDetailType;

    public SpeedOverallEntity getSpeedOverallEntity() {
        return mSpeedOverallEntity;
    }

    public void setSpeedOverallEntity(SpeedOverallEntity speedOverallEntity) {
        this.mSpeedOverallEntity = speedOverallEntity;
    }

    public SapaInfoEntity getSapaInfoEntity() {
        return mSapaInfoEntity;
    }

    public void setSapaInfoEntity(SapaInfoEntity sapaInfoEntity) {
        this.mSapaInfoEntity = sapaInfoEntity;
    }

    public NaviEtaInfo getNaviEtaInfo() {
        return mNaviEtaInfo;
    }

    public void setNaviEtaInfo(NaviEtaInfo naviEtaInfo) {
        this.mNaviEtaInfo = naviEtaInfo;
    }

    public long getCrossProgress() {
        return mCrossProgress;
    }

    public void setCrossProgress(long crossProgress) {
        this.mCrossProgress = crossProgress;
    }

    public CrossImageEntity getCrossImageEntity() {
        return mCrossImageEntity;
    }

    public void setCrossImageEntity(CrossImageEntity crossImageEntity) {
        this.mCrossImageEntity = crossImageEntity;
    }

    public NaviTmcInfo getNaviTmcInfo() {
        return mNaviTmcInfo;
    }

    public void setNaviTmcInfo(NaviTmcInfo naviTmcInfo) {
        this.mNaviTmcInfo = naviTmcInfo;
    }

    public NaviManeuverInfo getNaviManeuverInfo() {
        return mNaviManeuverInfo;
    }

    public void setNaviManeuverInfo(NaviManeuverInfo naviManeuverInfo) {
        this.mNaviManeuverInfo = naviManeuverInfo;
    }

    public LaneInfoEntity getLaneInfo() {
        return mLaneInfo;
    }

    public void setLaneInfo(LaneInfoEntity laneInfo) {
        this.mLaneInfo = laneInfo;
    }

    public List<NaviViaEntity> getViaList() {
        return mViaList;
    }

    public void setViaList(List<NaviViaEntity> mViaList) {
        this.mViaList = mViaList;
    }

    public NaviViaEntity getNaviViaEntity() {
        return mNaviViaEntity;
    }

    public void setNaviViaEntity(NaviViaEntity mNaviViaEntity) {
        this.mNaviViaEntity = mNaviViaEntity;
    }

    public boolean isIsViaDetailShow() {
        return mIsViaDetailShow;
    }

    public void setIsViaDetailShow(boolean mIsViaDetailShow) {
        this.mIsViaDetailShow = mIsViaDetailShow;
    }

    public int getSapaDetailType() {
        return mSapaDetailType;
    }

    public void setSapaDetailType(int mSapaDetailType) {
        this.mSapaDetailType = mSapaDetailType;
    }
}

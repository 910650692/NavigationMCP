package com.fy.navi.vrbridge.bean;

public final class MapState {

    public static MapState getInstance() {
        return MAP_STATE;
    }

    private static final MapState MAP_STATE = new MapState();

    private MapState() {
        mOpenStatus = false;
        mIsFront = false;
        mIsGPSNavi = false;
        mIsCruiseNavi = false;
        mCurrMapMode = -1;
        mIsMute = false;
        mBroadcastMode = -1;
        mIsRoutePage = false;
        mPathCount = -1;
        mViaPointsCount = 0;
        mViaPointsMaxCount = 5;
        mIsSetHome = false;
        mIsSetCompany = false;
        mIsRoadEvent = false;
        mIsOverView = false;
        mIsParallelFlagMain = -1;
        mSwitchParallelFlag = false;
        mIsParallelBridge = -1;
        mSwitchParallelBridge = false;
        mIsListPage = false;
        mCurrPlanPref = -1;
        mMaxZoomLevel = 19;
        mMinZoomLevel = 2;
        mIsDayWithMapStyle = true;
        mHasPrivacyPermission = false;
        mMaxVolumeLevel = 100;
        mIsLogin = false;
        mIsAgreeTeamAgreement = false;
        mIsInTeam = false;
        mIsTeamLeader = false;
    }

    private boolean mOpenStatus; //导航app是否打开，boolean值 - true:是 - false:否（默认）
    private boolean mIsFront; //是否是最上层app，boolean值 - true:是 - false:否（默认）
    private boolean mIsGPSNavi; //是否在导航中，boolean值 - true:是 - false:否（默认）
    private boolean mIsCruiseNavi; //是否在巡航中，boolean值 - true:是 - false:否（默认）
    private int mCurrMapMode; //地图视角，0:车头向上 - 1:正北向上 - 2:3D车头向上 - -1（默认）
    private boolean mIsMute; //播报是否静音，-true:是  -false:否（默认）
    private int mBroadcastMode; //- 0:标准/详细 - 1:简洁 - 2:静音 - 3:提示音 - 4:不播报模式 - 5:极简 - -1（默认）
    private boolean mIsRoutePage; //是否处于规划路线状态，boolean值 - true:是 - false:否（默认）
    private int mPathCount; //规划路线数量
    private int mViaPointsCount; //已添加的途经点数量
    private int mViaPointsMaxCount; //可以添加途经点的最大数量
    private boolean mIsSetHome; //是否设置家的地址，boolean值 - true:是 - false:否（默认）
    private boolean mIsSetCompany; //是否设置工作的地址，boolean值 - true:是 - false:否（默认）
    private boolean mIsRoadEvent; //路况状态是否开启，boolean值 - true:是 - false:否（默认）
    private boolean mIsOverView; //全览模式是否开启，boolean值 - true:是 - false:否（默认）
    private int mIsParallelFlagMain; //是否在主路，int值 - 0:主路 - 1:辅路 - -1（默认）
    private boolean mSwitchParallelFlag; //是否可以切换主辅路，boolean值 - true:是 - false:否（默认）
    private int mIsParallelBridge; //是否在桥上，int值 - 0:桥上 - 1:桥下 - -1（默认）
    private boolean mSwitchParallelBridge; //是否可以切换桥上桥下，boolean值 - true:是 - false:否（默认）
    private boolean mIsListPage; //当前列表是否显示中 - true:是 - false:否（默认）
    private int mCurrPlanPref;  //当前选择的路线偏好，int值 - 1:智能推荐 - 4:不走高速 - 8:少收费 - 16:躲避拥堵 - 256:时间优先 - 512:高速优先 - 1024:大路优先 - -1（默认）
    private int mMaxZoomLevel; //地图视图最大值
    private int mMinZoomLevel; //地图视图最小值
    private int mCurrZoomLevel; //当前地图视图大小，int值 - -1（默认）
    private String mEndPoiName; //导航目的地名字
    private String mEndPoiCity; //导航目的地所在城市
    private boolean mIsDayWithMapStyle; //白天黑夜模式，-true:白天（默认）  - false:黑夜
    private boolean mHasPrivacyPermission; //是否同意隐私协议，- true:是 - false:否（默认）
    private int mMaxVolumeLevel; //最大导航音量
    private int mCurrentVolumeLevel; //当前导航音量
    private boolean mIsLogin; //是否已登录
    private boolean mIsAgreeTeamAgreement; //是否已同意组队协议条款，暂不支持组队，false
    private boolean mIsInTeam;  //是否已在组队队伍，同上
    private boolean mIsTeamLeader; //是否组队队伍的队长，同上


    public boolean isOpenStatus() {
        return mOpenStatus;
    }

    public void setOpenStatus(final boolean openStatus) {
        this.mOpenStatus = openStatus;
    }

    public boolean isFront() {
        return mIsFront;
    }

    public void setFront(final boolean front) {
        mIsFront = front;
    }

    public boolean isGPSNavi() {
        return mIsGPSNavi;
    }

    public void setGPSNavi(final boolean gpsNavi) {
        mIsGPSNavi = gpsNavi;
    }

    public boolean isCruiseNavi() {
        return mIsCruiseNavi;
    }

    public void setCruiseNavi(final boolean cruiseNavi) {
        mIsCruiseNavi = cruiseNavi;
    }

    public int getCurrMapMode() {
        return mCurrMapMode;
    }

    public void setCurrMapMode(final int currMapMode) {
        this.mCurrMapMode = currMapMode;
    }

    public boolean isMute() {
        return mIsMute;
    }

    public void setMute(final boolean mute) {
        mIsMute = mute;
    }

    public int getBroadcastMode() {
        return mBroadcastMode;
    }

    public void setBroadcastMode(final int broadcastMode) {
        this.mBroadcastMode = broadcastMode;
    }

    public boolean isRoutePage() {
        return mIsRoutePage;
    }

    public void setRoutePage(final boolean routePage) {
        mIsRoutePage = routePage;
    }

    public int getPathCount() {
        return mPathCount;
    }

    public void setPathCount(final int pathCount) {
        this.mPathCount = pathCount;
    }

    public int getViaPointsCount() {
        return mViaPointsCount;
    }

    public void setViaPointsCount(final int viaPointsCount) {
        this.mViaPointsCount = viaPointsCount;
    }

    public int getViaPointsMaxCount() {
        return mViaPointsMaxCount;
    }

    public void setViaPointsMaxCount(final int viaPointsMaxCount) {
        this.mViaPointsMaxCount = viaPointsMaxCount;
    }

    public boolean isSetHome() {
        return mIsSetHome;
    }

    public void setSetHome(final boolean setHome) {
        mIsSetHome = setHome;
    }

    public boolean isSetCompany() {
        return mIsSetCompany;
    }

    public void setSetCompany(final boolean setCompany) {
        mIsSetCompany = setCompany;
    }

    public boolean isRoadEvent() {
        return mIsRoadEvent;
    }

    public void setRoadEvent(final boolean roadEvent) {
        mIsRoadEvent = roadEvent;
    }

    public boolean isOverView() {
        return mIsOverView;
    }

    public void setOverView(final boolean overView) {
        mIsOverView = overView;
    }

    public int isParallelFlagMain() {
        return mIsParallelFlagMain;
    }

    public void setParallelFlagMain(final int parallelFlagMain) {
        mIsParallelFlagMain = parallelFlagMain;
    }

    public boolean isSwitchParallelFlag() {
        return mSwitchParallelFlag;
    }

    public void setSwitchParallelFlag(final boolean switchParallelFlag) {
        this.mSwitchParallelFlag = switchParallelFlag;
    }

    public int isParallelBridge() {
        return mIsParallelBridge;
    }

    public void setParallelBridge(final int parallelBridge) {
        mIsParallelBridge = parallelBridge;
    }

    public boolean isSwitchParallelBridge() {
        return mSwitchParallelBridge;
    }

    public void setSwitchParallelBridge(final boolean switchParallelBridge) {
        this.mSwitchParallelBridge = switchParallelBridge;
    }

    public boolean isListPage() {
        return mIsListPage;
    }

    public void setListPage(final boolean listPage) {
        mIsListPage = listPage;
    }

    public int getCurrPlanPref() {
        return mCurrPlanPref;
    }

    public void setCurrPlanPref(final int currPlanPref) {
        this.mCurrPlanPref = currPlanPref;
    }

    public int getMaxZoomLevel() {
        return mMaxZoomLevel;
    }

    public void setMaxZoomLevel(final int maxZoomLevel) {
        this.mMaxZoomLevel = maxZoomLevel;
    }

    public int getMinZoomLevel() {
        return mMinZoomLevel;
    }

    public void setMinZoomLevel(final int minZoomLevel) {
        this.mMinZoomLevel = minZoomLevel;
    }

    public int getCurrZoomLevel() {
        return mCurrZoomLevel;
    }

    public void setCurrZoomLevel(final int currZoomLevel) {
        this.mCurrZoomLevel = currZoomLevel;
    }

    public String getEndPoiName() {
        return mEndPoiName;
    }

    public void setEndPoiName(final String endPoiName) {
        this.mEndPoiName = endPoiName;
    }

    public String getEndPoiCity() {
        return mEndPoiCity;
    }

    public void setEndPoiCity(final String endPoiCity) {
        this.mEndPoiCity = endPoiCity;
    }

    public boolean isDayWithMapStyle() {
        return mIsDayWithMapStyle;
    }

    public void setDayWithMapStyle(final boolean dayWithMapStyle) {
        mIsDayWithMapStyle = dayWithMapStyle;
    }

    public boolean isHasPrivacyPermission() {
        return mHasPrivacyPermission;
    }

    public void setHasPrivacyPermission(final boolean hasPrivacyPermission) {
        this.mHasPrivacyPermission = hasPrivacyPermission;
    }

    public int getMaxVolumeLevel() {
        return mMaxVolumeLevel;
    }

    public void setMaxVolumeLevel(final int maxVolumeLevel) {
        this.mMaxVolumeLevel = maxVolumeLevel;
    }

    public int getCurrentVolumeLevel() {
        return mCurrentVolumeLevel;
    }

    public void setCurrentVolumeLevel(final int currentVolumeLevel) {
        this.mCurrentVolumeLevel = currentVolumeLevel;
    }

    public boolean isLogin() {
        return mIsLogin;
    }

    public void setLogin(final boolean login) {
        mIsLogin = login;
    }

    public boolean isAgreeTeamAgreement() {
        return mIsAgreeTeamAgreement;
    }

    public void setAgreeTeamAgreement(final boolean agreeTeamAgreement) {
        mIsAgreeTeamAgreement = agreeTeamAgreement;
    }

    public boolean isInTeam() {
        return mIsInTeam;
    }

    public void setInTeam(final boolean inTeam) {
        mIsInTeam = inTeam;
    }

    public boolean isTeamLeader() {
        return mIsTeamLeader;
    }

    public void setTeamLeader(final boolean teamLeader) {
        mIsTeamLeader = teamLeader;
    }

    public static class Builder {
        private final MapState mapState = getInstance();

        /**
         * setOpenStatus
         * @param openStatus openStatus
         * @return Builder
         */
        public Builder setOpenStatus(final boolean openStatus) {
            mapState.setOpenStatus(openStatus);
            return this;
        }

        /**
         * setFront
         * @param front front
         * @return Builder
         */
        public Builder setFront(final boolean front) {
            mapState.setFront(front);
            return this;
        }

        /**
         * setGPSNavi
         * @param gpsNavi gpsNavi
         * @return Builder
         */
        public Builder setGPSNavi(final boolean gpsNavi) {
            mapState.setGPSNavi(gpsNavi);
            return this;
        }

        /**
         * setCruiseNavi
         * @param cruiseNavi cruiseNavi
         * @return Builder
         */
        public Builder setCruiseNavi(final boolean cruiseNavi) {
            mapState.setCruiseNavi(cruiseNavi);
            return this;
        }

        /**
         * setCurrMapMode
         * @param currMapMode currMapMode
         * @return Builder
         */
        public Builder setCurrMapMode(final int currMapMode) {
            mapState.setCurrMapMode(currMapMode);
            return this;
        }

        /**
         * setMute
         * @param mute mute
         * @return Builder
         */
        public Builder setMute(final boolean mute) {
            mapState.setMute(mute);
            return this;
        }
        /**
         * setBroadcastMode
         * @param broadcastMode broadcastMode
         * @return Builder
         */
        public Builder setBroadcastMode(final int broadcastMode) {
            mapState.setBroadcastMode(broadcastMode);
            return this;
        }
        /**
         * setRoutePage
         * @param routePage routePage
         * @return Builder
         */
        public Builder setRoutePage(final boolean routePage) {
            mapState.setRoutePage(routePage);
            return this;
        }
        /**
         * setPathCount
         * @param pathCount pathCount
         * @return Builder
         */
        public Builder setPathCount(final int pathCount) {
            mapState.setPathCount(pathCount);
            return this;
        }
        /**
         * setViaPointsCount
         * @param viaPointsCount viaPointsCount
         * @return Builder
         */
        public Builder setViaPointsCount(final int viaPointsCount) {
            mapState.setViaPointsCount(viaPointsCount);
            return this;
        }
        /**
         * setViaPointsMaxCount
         * @param viaPointsMaxCount viaPointsMaxCount
         * @return Builder
         */
        public Builder setViaPointsMaxCount(final int viaPointsMaxCount) {
            mapState.setViaPointsMaxCount(viaPointsMaxCount);
            return this;
        }
        /**
         * setSetHome
         * @param setHome setHome
         * @return Builder
         */
        public Builder setSetHome(final boolean setHome) {
            mapState.setSetHome(setHome);
            return this;
        }
        /**
         * setSetCompany
         * @param setCompany setCompany
         * @return Builder
         */
        public Builder setSetCompany(final boolean setCompany) {
            mapState.setSetCompany(setCompany);
            return this;
        }
        /**
         * setRoadEvent
         * @param roadEvent roadEvent
         * @return Builder
         */
        public Builder setRoadEvent(final boolean roadEvent) {
            mapState.setRoadEvent(roadEvent);
            return this;
        }
        /**
         * setOverView
         * @param overView overView
         * @return Builder
         */
        public Builder setOverView(final boolean overView) {
            mapState.setOverView(overView);
            return this;
        }
        /**
         * setParallelFlagMain
         * @param parallelFlagMain parallelFlagMain
         * @return Builder
         */
        public Builder setParallelFlagMain(final int parallelFlagMain) {
            mapState.setParallelFlagMain(parallelFlagMain);
            return this;
        }
        /**
         * setSwitchParallelFlag
         * @param switchParallelFlag switchParallelFlag
         * @return Builder
         */
        public Builder setSwitchParallelFlag(final boolean switchParallelFlag) {
            mapState.setSwitchParallelFlag(switchParallelFlag);
            return this;
        }
        /**
         * setParallelBridge
         * @param parallelBridge parallelBridge
         * @return Builder
         */
        public Builder setParallelBridge(final int parallelBridge) {
            mapState.setParallelBridge(parallelBridge);
            return this;
        }
        /**
         * setSwitchParallelBridge
         * @param switchParallelBridge switchParallelBridge
         * @return Builder
         */
        public Builder setSwitchParallelBridge(final boolean switchParallelBridge) {
            mapState.setSwitchParallelBridge(switchParallelBridge);
            return this;
        }
        /**
         * setListPage
         * @param listPage listPage
         * @return Builder
         */
        public Builder setListPage(final boolean listPage) {
            mapState.setListPage(listPage);
            return this;
        }
        /**
         * setCurrPlanPref
         * @param currPlanPref currPlanPref
         * @return Builder
         */
        public Builder setCurrPlanPref(final int currPlanPref) {
            mapState.setCurrPlanPref(currPlanPref);
            return this;
        }
        /**
         * setMaxZoomLevel
         * @param maxZoomLevel maxZoomLevel
         * @return Builder
         */
        public Builder setMaxZoomLevel(final int maxZoomLevel) {
            mapState.setMaxZoomLevel(maxZoomLevel);
            return this;
        }
        /**
         * setMinZoomLevel
         * @param minZoomLevel minZoomLevel
         * @return Builder
         */
        public Builder setMinZoomLevel(final int minZoomLevel) {
            mapState.setMinZoomLevel(minZoomLevel);
            return this;
        }
        /**
         * setCurrZoomLevel
         * @param currZoomLevel currZoomLevel
         * @return Builder
         */
        public Builder setCurrZoomLevel(final int currZoomLevel) {
            mapState.setCurrZoomLevel(currZoomLevel);
            return this;
        }
        /**
         * setEndPoiName
         * @param endPoiName endPoiName
         * @return Builder
         */
        public Builder setEndPoiName(final String endPoiName) {
            mapState.setEndPoiName(endPoiName);
            return this;
        }
        /**
         * setEndPoiCity
         * @param endPoiCity endPoiCity
         * @return Builder
         */
        public Builder setEndPoiCity(final String endPoiCity) {
            mapState.setEndPoiCity(endPoiCity);
            return this;
        }
        /**
         * setDayWithMapStyle
         * @param dayWithMapStyle dayWithMapStyle
         * @return Builder
         */
        public Builder setDayWithMapStyle(final boolean dayWithMapStyle) {
            mapState.setDayWithMapStyle(dayWithMapStyle);
            return this;
        }
        /**
         * setHasPrivacyPermission
         * @param hasPrivacyPermission hasPrivacyPermission
         * @return Builder
         */
        public Builder setHasPrivacyPermission(final boolean hasPrivacyPermission) {
            mapState.setHasPrivacyPermission(hasPrivacyPermission);
            return this;
        }
        /**
         * setMaxVolumeLevel
         * @param maxVolumeLevel maxVolumeLevel
         * @return Builder
         */
        public Builder setMaxVolumeLevel(final int maxVolumeLevel) {
            mapState.setMaxVolumeLevel(maxVolumeLevel);
            return this;
        }
        /**
         * setCurrentVolumeLevel
         * @param currentVolumeLevel currentVolumeLevel
         * @return Builder
         */
        public Builder setCurrentVolumeLevel(final int currentVolumeLevel) {
            mapState.setCurrentVolumeLevel(currentVolumeLevel);
            return this;
        }
        /**
         * setLogin
         * @param login login
         * @return Builder
         */
        public Builder setLogin(final boolean login) {
            mapState.setLogin(login);
            return this;
        }
        /**
         * setAgreeTeamAgreement
         * @param agreeTeamAgreement agreeTeamAgreement
         * @return Builder
         */
        public Builder setAgreeTeamAgreement(final boolean agreeTeamAgreement) {
            mapState.setAgreeTeamAgreement(agreeTeamAgreement);
            return this;
        }
        /**
         * setInTeam
         * @param inTeam inTeam
         * @return Builder
         */
        public Builder setInTeam(final boolean inTeam) {
            mapState.setInTeam(inTeam);
            return this;
        }
        /**
         * setTeamLeader
         * @param teamLeader teamLeader
         * @return Builder
         */
        public Builder setTeamLeader(final boolean teamLeader) {
            mapState.setTeamLeader(teamLeader);
            return this;
        }
        /**
         * build
         * @return MapState
         */
        public MapState build() {
            return mapState;
        }
    }

}

package com.fy.navi.vrbridge.bean;

public class MapState {



    public static MapState getInstance() {
        return mapState;
    }

    private static final MapState mapState = new MapState();

    private MapState() {
        openStatus = false;
        isFront = false;
        isGPSNavi = false;
        isCruiseNavi = false;
        currMapMode = -1;
        isMute = false;
        broadcastMode = -1;
        isRoutePage = false;
        pathCount = -1;
        viaPointsCount = 0;
        viaPointsMaxCount = 5;
        isSetHome = false;
        isSetCompany = false;
        isRoadEvent = false;
        isOverView = false;
        isParallelFlagMain = -1;
        switchParallelFlag = false;
        isParallelBridge = -1;
        switchParallelBridge = false;
        isListPage = false;
        currPlanPref = -1;
        maxZoomLevel = 19;
        minZoomLevel = 2;
        isDayWithMapStyle = true;
        hasPrivacyPermission = false;
        maxVolumeLevel = 100;
        isLogin = false;
        isAgreeTeamAgreement = false;
        isInTeam = false;
        isTeamLeader = false;
    }

    private boolean openStatus; //导航app是否打开，boolean值 - true:是 - false:否（默认）
    private boolean isFront; //是否是最上层app，boolean值 - true:是 - false:否（默认）
    private boolean isGPSNavi; //是否在导航中，boolean值 - true:是 - false:否（默认）
    private boolean isCruiseNavi; //是否在巡航中，boolean值 - true:是 - false:否（默认）
    private int currMapMode; //地图视角，0:车头向上 - 1:正北向上 - 2:3D车头向上 - -1（默认）
    private boolean isMute; //播报是否静音，-true:是  -false:否（默认）
    private int broadcastMode; //- 0:标准/详细 - 1:简洁 - 2:静音 - 3:提示音 - 4:不播报模式 - 5:极简 - -1（默认）
    private boolean isRoutePage; //是否处于规划路线状态，boolean值 - true:是 - false:否（默认）
    private int pathCount; //规划路线数量
    private int viaPointsCount; //已添加的途经点数量
    private int viaPointsMaxCount; //可以添加途经点的最大数量
    private boolean isSetHome; //是否设置家的地址，boolean值 - true:是 - false:否（默认）
    private boolean isSetCompany; //是否设置工作的地址，boolean值 - true:是 - false:否（默认）
    private boolean isRoadEvent; //路况状态是否开启，boolean值 - true:是 - false:否（默认）
    private boolean isOverView; //全览模式是否开启，boolean值 - true:是 - false:否（默认）
    private int isParallelFlagMain; //是否在主路，int值 - 0:主路 - 1:辅路 - -1（默认）
    private boolean switchParallelFlag; //是否可以切换主辅路，boolean值 - true:是 - false:否（默认）
    private int isParallelBridge; //是否在桥上，int值 - 0:桥上 - 1:桥下 - -1（默认）
    private boolean switchParallelBridge; //是否可以切换桥上桥下，boolean值 - true:是 - false:否（默认）
    private boolean isListPage; //当前列表是否显示中 - true:是 - false:否（默认）
    private int currPlanPref;  //当前选择的路线偏好，int值 - 1:智能推荐 - 4:不走高速 - 8:少收费 - 16:躲避拥堵 - 256:时间优先 - 512:高速优先 - 1024:大路优先 - -1（默认）
    private int maxZoomLevel; //地图视图最大值
    private int minZoomLevel; //地图视图最小值
    private int currZoomLevel; //当前地图视图大小，int值 - -1（默认）
    private String endPoiName; //导航目的地名字
    private String endPoiCity; //导航目的地所在城市
    private boolean isDayWithMapStyle; //白天黑夜模式，-true:白天（默认）  - false:黑夜
    private boolean hasPrivacyPermission; //是否同意隐私协议，- true:是 - false:否（默认）
    private int maxVolumeLevel; //最大导航音量
    private int currentVolumeLevel; //当前导航音量
    private boolean isLogin; //是否已登录
    private boolean isAgreeTeamAgreement; //是否已同意组队协议条款，暂不支持组队，false
    private boolean isInTeam;  //是否已在组队队伍，同上
    private boolean isTeamLeader; //是否组队队伍的队长，同上


    public boolean isOpenStatus() {
        return openStatus;
    }

    public void setOpenStatus(boolean openStatus) {
        this.openStatus = openStatus;
    }

    public boolean isFront() {
        return isFront;
    }

    public void setFront(boolean front) {
        isFront = front;
    }

    public boolean isGPSNavi() {
        return isGPSNavi;
    }

    public void setGPSNavi(boolean GPSNavi) {
        isGPSNavi = GPSNavi;
    }

    public boolean isCruiseNavi() {
        return isCruiseNavi;
    }

    public void setCruiseNavi(boolean cruiseNavi) {
        isCruiseNavi = cruiseNavi;
    }

    public int getCurrMapMode() {
        return currMapMode;
    }

    public void setCurrMapMode(int currMapMode) {
        this.currMapMode = currMapMode;
    }

    public boolean isMute() {
        return isMute;
    }

    public void setMute(boolean mute) {
        isMute = mute;
    }

    public int getBroadcastMode() {
        return broadcastMode;
    }

    public void setBroadcastMode(int broadcastMode) {
        this.broadcastMode = broadcastMode;
    }

    public boolean isRoutePage() {
        return isRoutePage;
    }

    public void setRoutePage(boolean routePage) {
        isRoutePage = routePage;
    }

    public int getPathCount() {
        return pathCount;
    }

    public void setPathCount(int pathCount) {
        this.pathCount = pathCount;
    }

    public int getViaPointsCount() {
        return viaPointsCount;
    }

    public void setViaPointsCount(int viaPointsCount) {
        this.viaPointsCount = viaPointsCount;
    }

    public int getViaPointsMaxCount() {
        return viaPointsMaxCount;
    }

    public void setViaPointsMaxCount(int viaPointsMaxCount) {
        this.viaPointsMaxCount = viaPointsMaxCount;
    }

    public boolean isSetHome() {
        return isSetHome;
    }

    public void setSetHome(boolean setHome) {
        isSetHome = setHome;
    }

    public boolean isSetCompany() {
        return isSetCompany;
    }

    public void setSetCompany(boolean setCompany) {
        isSetCompany = setCompany;
    }

    public boolean isRoadEvent() {
        return isRoadEvent;
    }

    public void setRoadEvent(boolean roadEvent) {
        isRoadEvent = roadEvent;
    }

    public boolean isOverView() {
        return isOverView;
    }

    public void setOverView(boolean overView) {
        isOverView = overView;
    }

    public int isParallelFlagMain() {
        return isParallelFlagMain;
    }

    public void setParallelFlagMain(int parallelFlagMain) {
        isParallelFlagMain = parallelFlagMain;
    }

    public boolean isSwitchParallelFlag() {
        return switchParallelFlag;
    }

    public void setSwitchParallelFlag(boolean switchParallelFlag) {
        this.switchParallelFlag = switchParallelFlag;
    }

    public int isParallelBridge() {
        return isParallelBridge;
    }

    public void setParallelBridge(int parallelBridge) {
        isParallelBridge = parallelBridge;
    }

    public boolean isSwitchParallelBridge() {
        return switchParallelBridge;
    }

    public void setSwitchParallelBridge(boolean switchParallelBridge) {
        this.switchParallelBridge = switchParallelBridge;
    }

    public boolean isListPage() {
        return isListPage;
    }

    public void setListPage(boolean listPage) {
        isListPage = listPage;
    }

    public int getCurrPlanPref() {
        return currPlanPref;
    }

    public void setCurrPlanPref(int currPlanPref) {
        this.currPlanPref = currPlanPref;
    }

    public int getMaxZoomLevel() {
        return maxZoomLevel;
    }

    public void setMaxZoomLevel(int maxZoomLevel) {
        this.maxZoomLevel = maxZoomLevel;
    }

    public int getMinZoomLevel() {
        return minZoomLevel;
    }

    public void setMinZoomLevel(int minZoomLevel) {
        this.minZoomLevel = minZoomLevel;
    }

    public int getCurrZoomLevel() {
        return currZoomLevel;
    }

    public void setCurrZoomLevel(int currZoomLevel) {
        this.currZoomLevel = currZoomLevel;
    }

    public String getEndPoiName() {
        return endPoiName;
    }

    public void setEndPoiName(String endPoiName) {
        this.endPoiName = endPoiName;
    }

    public String getEndPoiCity() {
        return endPoiCity;
    }

    public void setEndPoiCity(String endPoiCity) {
        this.endPoiCity = endPoiCity;
    }

    public boolean isDayWithMapStyle() {
        return isDayWithMapStyle;
    }

    public void setDayWithMapStyle(boolean dayWithMapStyle) {
        isDayWithMapStyle = dayWithMapStyle;
    }

    public boolean isHasPrivacyPermission() {
        return hasPrivacyPermission;
    }

    public void setHasPrivacyPermission(boolean hasPrivacyPermission) {
        this.hasPrivacyPermission = hasPrivacyPermission;
    }

    public int getMaxVolumeLevel() {
        return maxVolumeLevel;
    }

    public void setMaxVolumeLevel(int maxVolumeLevel) {
        this.maxVolumeLevel = maxVolumeLevel;
    }

    public int getCurrentVolumeLevel() {
        return currentVolumeLevel;
    }

    public void setCurrentVolumeLevel(int currentVolumeLevel) {
        this.currentVolumeLevel = currentVolumeLevel;
    }

    public boolean isLogin() {
        return isLogin;
    }

    public void setLogin(boolean login) {
        isLogin = login;
    }

    public boolean isAgreeTeamAgreement() {
        return isAgreeTeamAgreement;
    }

    public void setAgreeTeamAgreement(boolean agreeTeamAgreement) {
        isAgreeTeamAgreement = agreeTeamAgreement;
    }

    public boolean isInTeam() {
        return isInTeam;
    }

    public void setInTeam(boolean inTeam) {
        isInTeam = inTeam;
    }

    public boolean isTeamLeader() {
        return isTeamLeader;
    }

    public void setTeamLeader(boolean teamLeader) {
        isTeamLeader = teamLeader;
    }

    public static class Builder {
        private final MapState mapState = getInstance();

        public Builder setOpenStatus(boolean openStatus) {
            mapState.setOpenStatus(openStatus);
            return this;
        }

        public Builder setFront(boolean front) {
            mapState.setFront(front);
            return this;
        }

        public Builder setGPSNavi(boolean GPSNavi) {
            mapState.setGPSNavi(GPSNavi);
            return this;
        }

        public Builder setCruiseNavi(boolean cruiseNavi) {
            mapState.setCruiseNavi(cruiseNavi);
            return this;
        }

        public Builder setCurrMapMode(int currMapMode) {
            mapState.setCurrMapMode(currMapMode);
            return this;
        }

        public Builder setMute(boolean mute) {
            mapState.setMute(mute);
            return this;
        }

        public Builder setBroadcastMode(int broadcastMode) {
            mapState.setBroadcastMode(broadcastMode);
            return this;
        }

        public Builder setRoutePage(boolean routePage) {
            mapState.setRoutePage(routePage);
            return this;
        }

        public Builder setPathCount(int pathCount) {
            mapState.setPathCount(pathCount);
            return this;
        }

        public Builder setViaPointsCount(int viaPointsCount) {
            mapState.setViaPointsCount(viaPointsCount);
            return this;
        }

        public Builder setViaPointsMaxCount(int viaPointsMaxCount) {
            mapState.setViaPointsMaxCount(viaPointsMaxCount);
            return this;
        }

        public Builder setSetHome(boolean setHome) {
            mapState.setSetHome(setHome);
            return this;
        }

        public Builder setSetCompany(boolean setCompany) {
            mapState.setSetCompany(setCompany);
            return this;
        }

        public Builder setRoadEvent(boolean roadEvent) {
            mapState.setRoadEvent(roadEvent);
            return this;
        }

        public Builder setOverView(boolean overView) {
            mapState.setOverView(overView);
            return this;
        }

        public Builder setParallelFlagMain(int parallelFlagMain) {
            mapState.setParallelFlagMain(parallelFlagMain);
            return this;
        }

        public Builder setSwitchParallelFlag(boolean switchParallelFlag) {
            mapState.setSwitchParallelFlag(switchParallelFlag);
            return this;
        }

        public Builder setParallelBridge(int parallelBridge) {
            mapState.setParallelBridge(parallelBridge);
            return this;
        }

        public Builder setSwitchParallelBridge(boolean switchParallelBridge) {
            mapState.setSwitchParallelBridge(switchParallelBridge);
            return this;
        }

        public Builder setListPage(boolean listPage) {
            mapState.setListPage(listPage);
            return this;
        }

        public Builder setCurrPlanPref(int currPlanPref) {
            mapState.setCurrPlanPref(currPlanPref);
            return this;
        }

        public Builder setMaxZoomLevel(int maxZoomLevel) {
            mapState.setMaxZoomLevel(maxZoomLevel);
            return this;
        }

        public Builder setMinZoomLevel(int minZoomLevel) {
            mapState.setMinZoomLevel(minZoomLevel);
            return this;
        }

        public Builder setCurrZoomLevel(int currZoomLevel) {
            mapState.setCurrZoomLevel(currZoomLevel);
            return this;
        }

        public Builder setEndPoiName(String endPoiName) {
            mapState.setEndPoiName(endPoiName);
            return this;
        }

        public Builder setEndPoiCity(String endPoiCity) {
            mapState.setEndPoiCity(endPoiCity);
            return this;
        }

        public Builder setDayWithMapStyle(boolean dayWithMapStyle) {
            mapState.setDayWithMapStyle(dayWithMapStyle);
            return this;
        }

        public Builder setHasPrivacyPermission(boolean hasPrivacyPermission) {
            mapState.setHasPrivacyPermission(hasPrivacyPermission);
            return this;
        }

        public Builder setMaxVolumeLevel(int maxVolumeLevel) {
            mapState.setMaxVolumeLevel(maxVolumeLevel);
            return this;
        }

        public Builder setCurrentVolumeLevel(int currentVolumeLevel) {
            mapState.setCurrentVolumeLevel(currentVolumeLevel);
            return this;
        }

        public Builder setLogin(boolean login) {
            mapState.setLogin(login);
            return this;
        }

        public Builder setAgreeTeamAgreement(boolean agreeTeamAgreement) {
            mapState.setAgreeTeamAgreement(agreeTeamAgreement);
            return this;
        }

        public Builder setInTeam(boolean inTeam) {
            mapState.setInTeam(inTeam);
            return this;
        }

        public Builder setTeamLeader(boolean teamLeader) {
            mapState.setTeamLeader(teamLeader);
            return this;
        }

        public MapState build() {
            return mapState;
        }
    }

}

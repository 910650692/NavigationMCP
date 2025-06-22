package com.sgm.navi.service.define.navi;


import androidx.annotation.NonNull;

import com.sgm.navi.service.define.bean.GeoPoint;

import java.util.ArrayList;

public class NaviEtaInfo {
    //剩余时间/s
    private int mRemainTime;
    //剩余距离/m
    private int mRemainDist;
    //当前道路名
    private String mCurRouteName;
    /**
     * 当前路口转向ID 用来区分显示tbt板上的转向提示图标
     * 官方枚举类是 ManeuverIconID
     * 本地枚举类是 TbtIconAction/TbtExitIconAction
     */
    private int mCurManeuverID = -1;
    //下个道路名
    private String mNextRouteName;
    //下个路口转向ID
    private int mNextManeuverID = -1;
    //下个路口距离/m
    private int mNextDist;
    private int mRingOutCnt;
    // 线路唯一标识符
    public long pathID;

    /**
     * 导航类型
     * NaviTypeGPS 0:真实导航
     * NaviTypeSimulation 1:模拟导航
     * NaviTypeCruise 2:巡航模式
     * NaviTypeHealthRun 3:跑步模式
     * NaviTypeHealthRide 4:骑行模式
     * NaviTypeHealthShareBike 5:共享单车模式
     */
    public int type;
    public ArrayList<NaviTimeAndDist> viaRemain;
    public ArrayList<NaviTimeAndDist> ChargeStationRemain;
    public int routeRemainLightCount;
    public int linkRemainDist;
    public int curSegIdx;
    public int curLinkIdx;
    public int curPointIdx;
    public int curRoadClass;
    public int roundaboutOutAngle;
    public int driveTime;
    public int mDriveDist;
    public int cityCode;
    public int curLinkSpeed;
    public int segTipsDis;
    public NotAvoidInfo notAvoidInfo;
    public int crossManeuverID;
    public ArrayList<NaviCrossNaviInfo> nextCrossInfo;
    public int NaviInfoFlag;
    public ArrayList<NaviInfoPanel> NaviInfoData;
    public String gateName;
    public String aoiName;
    public boolean innerRoad;

    public NaviEtaInfo() {
        this.pathID = 0L;
        this.type = 0;
        this.viaRemain = new ArrayList();
        this.ChargeStationRemain = new ArrayList();
        this.routeRemainLightCount = 0;
        this.linkRemainDist = 0;
        this.curSegIdx = 0;
        this.curLinkIdx = 0;
        this.curPointIdx = 0;
        this.curRoadClass = -1;
        this.mCurRouteName = "";
        this.mRingOutCnt = 0;
        this.roundaboutOutAngle = 0;
        this.driveTime = 0;
        this.mDriveDist = 0;
        this.cityCode = 0;
        this.curLinkSpeed = 0;
        this.segTipsDis = 0;
        this.notAvoidInfo = new NotAvoidInfo();
        this.crossManeuverID = 0;
        this.nextCrossInfo = new ArrayList();
        this.NaviInfoFlag = 0;
        this.NaviInfoData = new ArrayList();
        this.gateName = "";
        this.aoiName = "";
        this.innerRoad = false;
    }

    public int getRemainTime() {
        return mRemainTime;
    }

    public void setRemainTime(int allTime) {
        this.mRemainTime = allTime;
    }

    public int getRemainDist() {
        return mRemainDist;
    }

    public void setRemainDist(int allDist) {
        this.mRemainDist = allDist;
    }

    public String getCurRouteName() {
        return mCurRouteName;
    }

    public void setCurRouteName(String curRouteName) {
        this.mCurRouteName = curRouteName;
    }

    public int getCurManeuverID() {
        return mCurManeuverID;
    }

    public void setCurManeuverID(int curManeuverID) {
        this.mCurManeuverID = curManeuverID;
    }

    public String getNextRouteName() {
        return mNextRouteName;
    }

    public void setNextRouteName(String nextRouteName) {
        this.mNextRouteName = nextRouteName;
    }

    public int getNextManeuverID() {
        return mNextManeuverID;
    }

    public void setNextManeuverID(int nextManeuverID) {
        this.mNextManeuverID = nextManeuverID;
    }

    public int getNextDist() {
        return mNextDist;
    }

    public void setNextDist(int nextDist) {
        this.mNextDist = nextDist;
    }

    public int getRingOutCnt() {
        return mRingOutCnt;
    }

    public void setRingOutCnt(int ringOutCnt) {
        this.mRingOutCnt = ringOutCnt;
    }

    public long getPathID() {
        return pathID;
    }

    public void setPathID(long pathID) {
        this.pathID = pathID;
    }

    public int getType() {
        return type;
    }

    public void setType(int type) {
        this.type = type;
    }

    public ArrayList<NaviTimeAndDist> getViaRemain() {
        return viaRemain;
    }

    public void setViaRemain(ArrayList<NaviTimeAndDist> viaRemain) {
        this.viaRemain = viaRemain;
    }

    public ArrayList<NaviTimeAndDist> getChargeStationRemain() {
        return ChargeStationRemain;
    }

    public void setChargeStationRemain(ArrayList<NaviTimeAndDist> chargeStationRemain) {
        ChargeStationRemain = chargeStationRemain;
    }

    public int getRouteRemainLightCount() {
        return routeRemainLightCount;
    }

    public void setRouteRemainLightCount(int routeRemainLightCount) {
        this.routeRemainLightCount = routeRemainLightCount;
    }

    public int getLinkRemainDist() {
        return linkRemainDist;
    }

    public void setLinkRemainDist(int linkRemainDist) {
        this.linkRemainDist = linkRemainDist;
    }

    public int getCurSegIdx() {
        return curSegIdx;
    }

    public void setCurSegIdx(int curSegIdx) {
        this.curSegIdx = curSegIdx;
    }

    public int getCurLinkIdx() {
        return curLinkIdx;
    }

    public void setCurLinkIdx(int curLinkIdx) {
        this.curLinkIdx = curLinkIdx;
    }

    public int getCurPointIdx() {
        return curPointIdx;
    }

    public void setCurPointIdx(int curPointIdx) {
        this.curPointIdx = curPointIdx;
    }

    public int getCurRoadClass() {
        return curRoadClass;
    }

    public void setCurRoadClass(int curRoadClass) {
        this.curRoadClass = curRoadClass;
    }

    public int getRoundaboutOutAngle() {
        return roundaboutOutAngle;
    }

    public void setRoundaboutOutAngle(int roundaboutOutAngle) {
        this.roundaboutOutAngle = roundaboutOutAngle;
    }

    public int getDriveTime() {
        return driveTime;
    }

    public void setDriveTime(int driveTime) {
        this.driveTime = driveTime;
    }

    public int getDriveDist() {
        return mDriveDist;
    }

    public void setDriveDist(int driveDist) {
        this.mDriveDist = driveDist;
    }

    public int getCityCode() {
        return cityCode;
    }

    public void setCityCode(int cityCode) {
        this.cityCode = cityCode;
    }

    public int getCurLinkSpeed() {
        return curLinkSpeed;
    }

    public void setCurLinkSpeed(int curLinkSpeed) {
        this.curLinkSpeed = curLinkSpeed;
    }

    public int getSegTipsDis() {
        return segTipsDis;
    }

    public void setSegTipsDis(int segTipsDis) {
        this.segTipsDis = segTipsDis;
    }

    public NotAvoidInfo getNotAvoidInfo() {
        return notAvoidInfo;
    }

    public void setNotAvoidInfo(NotAvoidInfo notAvoidInfo) {
        this.notAvoidInfo = notAvoidInfo;
    }

    public int getCrossManeuverID() {
        return crossManeuverID;
    }

    public void setCrossManeuverID(int crossManeuverID) {
        this.crossManeuverID = crossManeuverID;
    }

    public ArrayList<NaviCrossNaviInfo> getNextCrossInfo() {
        return nextCrossInfo;
    }

    public void setNextCrossInfo(ArrayList<NaviCrossNaviInfo> nextCrossInfo) {
        this.nextCrossInfo = nextCrossInfo;
    }

    public int getNaviInfoFlag() {
        return NaviInfoFlag;
    }

    public void setNaviInfoFlag(int naviInfoFlag) {
        NaviInfoFlag = naviInfoFlag;
    }

    public ArrayList<NaviInfoPanel> getNaviInfoData() {
        return NaviInfoData;
    }

    public void setNaviInfoData(ArrayList<NaviInfoPanel> naviInfoData) {
        NaviInfoData = naviInfoData;
    }

    public String getGateName() {
        return gateName;
    }

    public void setGateName(String gateName) {
        this.gateName = gateName;
    }

    public String getAoiName() {
        return aoiName;
    }

    public void setAoiName(String aoiName) {
        this.aoiName = aoiName;
    }

    public boolean isInnerRoad() {
        return innerRoad;
    }

    public void setInnerRoad(boolean innerRoad) {
        this.innerRoad = innerRoad;
    }

    @NonNull
    @Override
    public String toString() {
        return "NaviEtaInfo{" +
                "allTime=" + mRemainTime +
                ", allDist=" + mRemainDist +
                ", curRouteName='" + mCurRouteName + '\'' +
                ", curManeuverID=" + mCurManeuverID +
                ", nextRouteName='" + mNextRouteName + '\'' +
                ", nextManeuverID=" + mNextManeuverID +
                ", nextDist=" + mNextDist +
                ", ringOutCnt=" + mRingOutCnt +
                ", pathID=" + pathID +
                ", type=" + type +
                ", viaRemain=" + viaRemain +
                ", ChargeStationRemain=" + ChargeStationRemain +
                ", routeRemainLightCount=" + routeRemainLightCount +
                ", linkRemainDist=" + linkRemainDist +
                ", curSegIdx=" + curSegIdx +
                ", curLinkIdx=" + curLinkIdx +
                ", curPointIdx=" + curPointIdx +
                ", curRoadClass=" + curRoadClass +
                ", roundaboutOutAngle=" + roundaboutOutAngle +
                ", driveTime=" + driveTime +
                ", driveDist=" + mDriveDist +
                ", cityCode=" + cityCode +
                ", curLinkSpeed=" + curLinkSpeed +
                ", segTipsDis=" + segTipsDis +
                ", notAvoidInfo=" + notAvoidInfo +
                ", crossManeuverID=" + crossManeuverID +
                ", nextCrossInfo=" + nextCrossInfo +
                ", NaviInfoFlag=" + NaviInfoFlag +
                ", NaviInfoData=" + NaviInfoData +
                ", gateName='" + gateName + '\'' +
                ", aoiName='" + aoiName + '\'' +
                ", innerRoad=" + innerRoad +
                '}';
    }

    public static class NaviTimeAndDist {
        public int time;
        public int dist;

        public NaviTimeAndDist() {
        }

        public NaviTimeAndDist(int time, int dist) {
            this.time = time;
            this.dist = dist;
        }

        public int getDist() {
            return dist;
        }

        public void setDist(int dist) {
            this.dist = dist;
        }

        public int getTime() {
            return time;
        }

        public void setTime(int time) {
            this.time = time;
        }

        @NonNull
        @Override
        public String toString() {
            return "NaviTimeAndDist{" +
                    "time=" + time +
                    ", dist=" + dist +
                    '}';
        }
    }

    public static class NaviCrossNaviInfo {
        public long pathID;
        public int segIdx;
        public int mainAction;
        public int assistAction;
        public int maneuverID;
        public int crossManeuverID;
        public String nextRoadName;
        public int curToSegmentDist;
        public int curToSegmentTime;
        public short outCnt;
        public short viaNum;
        public short destDirection;
        public short tunnelFlag;
        public short reversed;
        public short rev;
        public int segmentIndex;
        public int linkIndex;

        public long getPathID() {
            return pathID;
        }

        public void setPathID(final long pathID) {
            this.pathID = pathID;
        }

        public int getSegIdx() {
            return segIdx;
        }

        public void setSegIdx(final int segIdx) {
            this.segIdx = segIdx;
        }

        public int getMainAction() {
            return mainAction;
        }

        public void setMainAction(final int mainAction) {
            this.mainAction = mainAction;
        }

        public int getAssistAction() {
            return assistAction;
        }

        public void setAssistAction(final int assistAction) {
            this.assistAction = assistAction;
        }

        public int getManeuverID() {
            return maneuverID;
        }

        public void setManeuverID(final int maneuverID) {
            this.maneuverID = maneuverID;
        }

        public int getCrossManeuverID() {
            return crossManeuverID;
        }

        public void setCrossManeuverID(final int crossManeuverID) {
            this.crossManeuverID = crossManeuverID;
        }

        public String getNextRoadName() {
            return nextRoadName;
        }

        public void setNextRoadName(final String nextRoadName) {
            this.nextRoadName = nextRoadName;
        }

        public int getCurToSegmentDist() {
            return curToSegmentDist;
        }

        public void setCurToSegmentDist(final int curToSegmentDist) {
            this.curToSegmentDist = curToSegmentDist;
        }

        public int getCurToSegmentTime() {
            return curToSegmentTime;
        }

        public void setCurToSegmentTime(final int curToSegmentTime) {
            this.curToSegmentTime = curToSegmentTime;
        }

        public short getOutCnt() {
            return outCnt;
        }

        public void setOutCnt(final short outCnt) {
            this.outCnt = outCnt;
        }

        public short getViaNum() {
            return viaNum;
        }

        public void setViaNum(final short viaNum) {
            this.viaNum = viaNum;
        }

        public short getDestDirection() {
            return destDirection;
        }

        public void setDestDirection(final short destDirection) {
            this.destDirection = destDirection;
        }

        public short getTunnelFlag() {
            return tunnelFlag;
        }

        public void setTunnelFlag(final short tunnelFlag) {
            this.tunnelFlag = tunnelFlag;
        }

        public short getReversed() {
            return reversed;
        }

        public void setReversed(final short reversed) {
            this.reversed = reversed;
        }

        public short getRev() {
            return rev;
        }

        public void setRev(final short rev) {
            this.rev = rev;
        }

        public int getSegmentIndex() {
            return segmentIndex;
        }

        public void setSegmentIndex(final int segmentIndex) {
            this.segmentIndex = segmentIndex;
        }

        public int getLinkIndex() {
            return linkIndex;
        }

        public void setLinkIndex(final int linkIndex) {
            this.linkIndex = linkIndex;
        }

        @NonNull
        @Override
        public String toString() {
            return "NaviCrossNaviInfo{" +
                    "pathID=" + pathID +
                    ", segIdx=" + segIdx +
                    ", mainAction=" + mainAction +
                    ", assistAction=" + assistAction +
                    ", maneuverID=" + maneuverID +
                    ", crossManeuverID=" + crossManeuverID +
                    ", nextRoadName='" + nextRoadName + '\'' +
                    ", curToSegmentDist=" + curToSegmentDist +
                    ", curToSegmentTime=" + curToSegmentTime +
                    ", outCnt=" + outCnt +
                    ", viaNum=" + viaNum +
                    ", destDirection=" + destDirection +
                    ", tunnelFlag=" + tunnelFlag +
                    ", reversed=" + reversed +
                    ", rev=" + rev +
                    ", segmentIndex=" + segmentIndex +
                    ", linkIndex=" + linkIndex +
                    '}';
        }
    }

    public static class NaviInfoPanel {
        public int maneuverID;
        public NaviTimeAndDist segmentRemain;
        public String nextRouteName;
        public int nextRoadNameSegIdx;
        public int nextRoadNameLinkIdx;
        public int split;

        public int getManeuverID() {
            return maneuverID;
        }

        public void setManeuverID(final int maneuverID) {
            this.maneuverID = maneuverID;
        }

        public NaviTimeAndDist getSegmentRemain() {
            return segmentRemain;
        }

        public void setSegmentRemain(final NaviTimeAndDist segmentRemain) {
            this.segmentRemain = segmentRemain;
        }

        public String getNextRouteName() {
            return nextRouteName;
        }

        public void setNextRouteName(final String nextRouteName) {
            this.nextRouteName = nextRouteName;
        }

        public int getNextRoadNameSegIdx() {
            return nextRoadNameSegIdx;
        }

        public void setNextRoadNameSegIdx(final int nextRoadNameSegIdx) {
            this.nextRoadNameSegIdx = nextRoadNameSegIdx;
        }

        public int getNextRoadNameLinkIdx() {
            return nextRoadNameLinkIdx;
        }

        public void setNextRoadNameLinkIdx(final int nextRoadNameLinkIdx) {
            this.nextRoadNameLinkIdx = nextRoadNameLinkIdx;
        }

        public int getSplit() {
            return split;
        }

        public void setSplit(final int split) {
            this.split = split;
        }

        @NonNull
        @Override
        public String toString() {
            return "NaviInfoPanel{" +
                    "maneuverID=" + maneuverID +
                    ", segmentRemain=" + segmentRemain +
                    ", nextRouteName='" + nextRouteName + '\'' +
                    ", nextRoadNameSegIdx=" + nextRoadNameSegIdx +
                    ", nextRoadNameLinkIdx=" + nextRoadNameLinkIdx +
                    ", split=" + split +
                    '}';
        }
    }

    /**
     * 导航过程中未避开的
     */
    public static class NotAvoidInfo {
        public int type;
        public int distToCar;
        public GeoPoint coord2D;
        public GeoPoint coord3D;
        public int forbidType;
        public boolean valid;

        public int getType() {
            return type;
        }

        public void setType(final int type) {
            this.type = type;
        }

        public int getDistToCar() {
            return distToCar;
        }

        public void setDistToCar(final int distToCar) {
            this.distToCar = distToCar;
        }

        public GeoPoint getCoord2D() {
            return coord2D;
        }

        public void setCoord2D(final GeoPoint coord2D) {
            this.coord2D = coord2D;
        }

        public GeoPoint getCoord3D() {
            return coord3D;
        }

        public void setCoord3D(final GeoPoint coord3D) {
            this.coord3D = coord3D;
        }

        public int getForbidType() {
            return forbidType;
        }

        public void setForbidType(final int forbidType) {
            this.forbidType = forbidType;
        }

        public boolean isValid() {
            return valid;
        }

        public void setValid(final boolean valid) {
            this.valid = valid;
        }

        @NonNull
        @Override
        public String toString() {
            return "NotAvoidInfo{" +
                    "type=" + type +
                    ", distToCar=" + distToCar +
                    ", coord2D=" + coord2D +
                    ", coord3D=" + coord3D +
                    ", forbidType=" + forbidType +
                    ", valid=" + valid +
                    '}';
        }
    }
}

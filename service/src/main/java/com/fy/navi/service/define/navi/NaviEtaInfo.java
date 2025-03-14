package com.fy.navi.service.define.navi;


import androidx.annotation.NonNull;

import com.fy.navi.service.define.bean.GeoPoint;

import java.util.ArrayList;

public class NaviEtaInfo {
    //剩余时间/s
    public int allTime;
    //剩余距离/m
    public int allDist;
    //当前道路名
    public String curRouteName;
    /**
     * 当前路口转向ID 用来区分显示tbt板上的转向提示图标
     * 官方枚举类是 ManeuverIconID
     * 本地枚举类是 TbtIconAction/TbtExitIconAction
     */
    public int curManeuverID = -1;
    //下个道路名
    public String nextRouteName;
    //下个路口转向ID
    public int nextManeuverID = -1;
    //下个路口距离/m
    public int nextDist;
    public int ringOutCnt;

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
    public int driveDist;
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
        this.curRouteName = "";
        this.ringOutCnt = 0;
        this.roundaboutOutAngle = 0;
        this.driveTime = 0;
        this.driveDist = 0;
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

    @NonNull
    @Override
    public String toString() {
        return "NaviEtaInfo{" +
                "allTime=" + allTime +
                ", allDist=" + allDist +
                ", curRouteName='" + curRouteName + '\'' +
                ", curManeuverID=" + curManeuverID +
                ", nextRouteName='" + nextRouteName + '\'' +
                ", nextManeuverID=" + nextManeuverID +
                ", nextDist=" + nextDist +
                ", ringOutCnt=" + ringOutCnt +
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
                ", driveDist=" + driveDist +
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

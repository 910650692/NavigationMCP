package com.fy.navi.service.define.navi;

import java.util.ArrayList;
import java.util.List;

import lombok.Getter;
import lombok.Setter;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/3/12
 */
@Setter
@Getter
public class L2NaviBean {

    /**
     * crossInfoData : {"hasTrafficLight":1,"highLightLaneTypes":[255,255,1,23,255,255],"highLightLanes":[0,0,1,1,0,0],"laneNum":6,"laneTypes":[1,1,1,23,0,23],"trafficLightPosition":51,"frontLaneType":[1,1,4,2,5,1],"backLaneType":[1,2,4,2,5,2],"timestamp":15687413,"segmentIndex":1,"linkIndex":0}
     * aheadIntersections : [{"highLightLaneTypes":[255,255,1,23,255,255],"laneNum":6,"laneTypes":[1,1,1,23,0,23],"frontLaneType":[1,1,4,2,5,1],"backLaneType":[1,2,4,2,5,2],"timestamp":15687413,"segmentIndex":1,"linkIndex":0}]
     * mixForks : [{"Distance":49,"Position":{"x":31.11555557,"y":121.60491889},"RoadClass":3,"SegmentIndex":25},{"Distance":24,"Position":{"x":31.11554557,"y":121.60492889},"RoadClass":3,"SegmentIndex":26}]
     * guidePointInfo : {"nextGuideDist":51,"nextGuideType":2}
     * hasTidalLane : 0
     * intervalCameraData : {"intervalCameraEndPointDist":0,"intervalCameraSpeedValue":0,"intervalCameraStartPointDist":0}
     * isServiceAreaRoad : 0
     * limitCameraData : {"spdLmtEleEyeDist":0,"spdLmtEleEyeSpeedValue":0}
     * rampDist : 65535
     * tollStationDist : 65535
     * tunnelInfo : {"tunnelDist":65535,"tunnelLength":0}
     * vehiclePosition : {"CurPathID":235451536,"currentSpeedLimit":0,"distToDestination":10138,"locationLatitude":31.115323055555557,"locationLinkIndex":0,"locationLinkOffset":71,"locationLongitude":121.60491888888889,"mainSideRots":0,"naviStatus":1,"roadClass":7,"roadOwnership":0,"ttsText":"左转,随后在分岔口走最右侧岔路,往秀浦路方向","formway":1,"linkType":1}
     * warningFacility : {"boardSignDist":0,"boardSignType":0,"limitSpeed":0}
     */

    private CrossInfoDataBean crossInfoData;
    private GuidePointInfoBean guidePointInfo;
    private int hasTidalLane;
    private IntervalCameraDataBean intervalCameraData;
    private int isServiceAreaRoad;
    private LimitCameraDataBean limitCameraData;
    private int rampDist;
    private int tollStationDist;
    private TunnelInfoBean tunnelInfo;
    private VehiclePositionBean vehiclePosition;
    private WarningFacilityBean warningFacility;
    private List<AheadIntersectionsBean> aheadIntersections;
    private List<MixForksBean> mixForks = new ArrayList<>();

    public L2NaviBean clear(){
        return new L2NaviBean();
    }

    @Setter
    @Getter
    public static class CrossInfoDataBean {
        /**
         * hasTrafficLight : 1
         * highLightLaneTypes : [255,255,1,23,255,255]
         * highLightLanes : [0,0,1,1,0,0]
         * laneNum : 6
         * laneTypes : [1,1,1,23,0,23]
         * trafficLightPosition : 51
         * frontLaneType : [1,1,4,2,5,1]
         * backLaneType : [1,2,4,2,5,2]
         * timestamp : 15687413
         * segmentIndex : 1
         * linkIndex : 0
         */

        private int hasTrafficLight;
        private int laneNum;
        private int trafficLightPosition;
        private int timestamp;
        private int segmentIndex;
        private int linkIndex;
        private List<Integer> highLightLaneTypes;
        private List<Integer> highLightLanes;
        private List<Integer> laneTypes;
        private List<Integer> frontLaneType;
        private List<Integer> backLaneType;

    }

    @Setter
    @Getter
    public static class GuidePointInfoBean {
        /**
         * nextGuideDist : 51
         * nextGuideType : 2
         */

        private int nextGuideDist;
        private int nextGuideType;

    }

    @Setter
    @Getter
    public static class IntervalCameraDataBean {
        /**
         * intervalCameraEndPointDist : 0
         * intervalCameraSpeedValue : 0
         * intervalCameraStartPointDist : 0
         */

        private int intervalCameraEndPointDist;
        private int intervalCameraSpeedValue;
        private int intervalCameraStartPointDist;

    }

    @Setter
    @Getter
    public static class LimitCameraDataBean {
        /**
         * spdLmtEleEyeDist : 0
         * spdLmtEleEyeSpeedValue : 0
         */

        private int spdLmtEleEyeDist;
        private int spdLmtEleEyeSpeedValue;

    }

    @Setter
    @Getter
    public static class TunnelInfoBean {
        /**
         * tunnelDist : 65535
         * tunnelLength : 0
         */

        private int tunnelDist;
        private int tunnelLength;

    }

    @Setter
    @Getter
    public static class VehiclePositionBean {
        /**
         * CurPathID : 235451536
         * currentSpeedLimit : 0
         * distToDestination : 10138
         * locationLatitude : 31.115323055555557
         * locationLinkIndex : 0
         * locationLinkOffset : 71
         * locationLongitude : 121.60491888888889
         * mainSideRots : 0
         * naviStatus : 1
         * roadClass : 7
         * roadOwnership : 0
         * ttsText : 左转,随后在分岔口走最右侧岔路,往秀浦路方向
         * formway : 1
         * linkType : 1
         */

        private int CurPathID;
        private int currentSpeedLimit;
        private int distToDestination;
        private double locationLatitude;
        private int locationLinkIndex;
        private int locationLinkOffset;
        private double locationLongitude;
        private int mainSideRots;
        private int naviStatus;
        private int roadClass;
        private int roadOwnership;
        private String ttsText;
        private int formway;
        private int linkType;

    }

    @Setter
    @Getter
    public static class WarningFacilityBean {
        /**
         * boardSignDist : 0
         * boardSignType : 0
         * limitSpeed : 0
         */

        private int boardSignDist;
        private int boardSignType;
        private int limitSpeed;

    }

    @Setter
    @Getter
    public static class AheadIntersectionsBean {
        /**
         * highLightLaneTypes : [255,255,1,23,255,255]
         * laneNum : 6
         * laneTypes : [1,1,1,23,0,23]
         * frontLaneType : [1,1,4,2,5,1]
         * backLaneType : [1,2,4,2,5,2]
         * timestamp : 15687413
         * segmentIndex : 1
         * linkIndex : 0
         */

        private int laneNum;
        private int timestamp;
        private int segmentIndex;
        private int linkIndex;
        private List<Integer> highLightLaneTypes;
        private List<Integer> laneTypes;
        private List<Integer> frontLaneType;
        private List<Integer> backLaneType;

    }

    @Setter
    @Getter
    public static class MixForksBean {
        /**
         * Distance : 49
         * Position : {"x":31.11555557,"y":121.60491889}
         * RoadClass : 3
         * SegmentIndex : 25
         */

        private int Distance;
        private PositionBean Position = new PositionBean();
        private int RoadClass;
        private int SegmentIndex;

        @Setter
        @Getter
        public static class PositionBean {
            /**
             * x : 31.11555557
             * y : 121.60491889
             */

            private double x;
            private double y;

        }
    }
}

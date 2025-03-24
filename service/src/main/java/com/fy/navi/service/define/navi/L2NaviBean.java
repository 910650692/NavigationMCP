package com.fy.navi.service.define.navi;

import androidx.annotation.NonNull;

import java.util.ArrayList;
import java.util.List;

import lombok.Getter;
import lombok.Setter;

/**
 * @author fy
 * @version $Revision.*$
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

    private CrossInfoDataBean mCrossInfoData;
    private GuidePointInfoBean mGuidePointInfo;
    private int mHasTidalLane; // 潮汐车道
    @Setter
    private IntervalCameraDataBean mIntervalCameraData;

    private int mIsServiceAreaRoad; // 是否在服务区内
    @Setter
    private LimitCameraDataBean mLimitCameraData;
    private int mRampDist;
    private int mTollStationDist; // 与前方收费站剩余距离
    private TunnelInfoBean mTunnelInfo;
    private VehiclePositionBean mVehiclePosition;
    private WarningFacilityBean mWarningFacility;
    private List<AheadIntersectionsBean> mAheadIntersections;
    private List<MixForksBean> mMixForks = new ArrayList<>();
    private EndParkingInfo mEndParkingInfo = new EndParkingInfo();

    /**
     * 清空所有数据
     */
    public void clear() {
        mCrossInfoData = new CrossInfoDataBean();
        mGuidePointInfo = new GuidePointInfoBean();
        mIntervalCameraData = new IntervalCameraDataBean();
        mLimitCameraData = new LimitCameraDataBean();
        mTunnelInfo = new TunnelInfoBean();
        mVehiclePosition = new VehiclePositionBean();
        mWarningFacility = new WarningFacilityBean();
        mEndParkingInfo = new EndParkingInfo();
        mAheadIntersections.clear();
        mMixForks.clear();
        mTollStationDist = 0;
        mHasTidalLane = 0;
        mIsServiceAreaRoad = 0;
        mRampDist = 0;
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

        private int mHasTrafficLight;
        private int mLaneNum;
        private int mTrafficLightPosition;
        private int mTimestamp;
        private int mSegmentIndex;
        private int mLinkIndex;
        private List<Integer> mHighLightLaneTypes;
        private List<Integer> mHighLightLanes;
        private List<Integer> mLaneTypes;
        private List<Integer> mFrontLaneType;
        private List<Integer> mBackLaneType;

        public int getHasTrafficLight() {
            return mHasTrafficLight;
        }

        public void setHasTrafficLight(final int hasTrafficLight) {
            this.mHasTrafficLight = hasTrafficLight;
        }

        public int getLaneNum() {
            return mLaneNum;
        }

        public void setLaneNum(final int laneNum) {
            this.mLaneNum = laneNum;
        }

        public int getTrafficLightPosition() {
            return mTrafficLightPosition;
        }

        public void setTrafficLightPosition(final int trafficLightPosition) {
            this.mTrafficLightPosition = trafficLightPosition;
        }

        public int getTimestamp() {
            return mTimestamp;
        }

        public void setTimestamp(final int timestamp) {
            this.mTimestamp = timestamp;
        }

        public int getSegmentIndex() {
            return mSegmentIndex;
        }

        public void setSegmentIndex(final int segmentIndex) {
            this.mSegmentIndex = segmentIndex;
        }

        public int getLinkIndex() {
            return mLinkIndex;
        }

        public void setLinkIndex(final int linkIndex) {
            this.mLinkIndex = linkIndex;
        }

        public List<Integer> getHighLightLaneTypes() {
            return mHighLightLaneTypes;
        }

        public void setHighLightLaneTypes(final List<Integer> highLightLaneTypes) {
            this.mHighLightLaneTypes = highLightLaneTypes;
        }

        public List<Integer> getHighLightLanes() {
            return mHighLightLanes;
        }

        public void setHighLightLanes(final List<Integer> highLightLanes) {
            this.mHighLightLanes = highLightLanes;
        }

        public List<Integer> getLaneTypes() {
            return mLaneTypes;
        }

        public void setLaneTypes(final List<Integer> laneTypes) {
            this.mLaneTypes = laneTypes;
        }

        public List<Integer> getFrontLaneType() {
            return mFrontLaneType;
        }

        public void setFrontLaneType(final List<Integer> frontLaneType) {
            this.mFrontLaneType = frontLaneType;
        }

        public List<Integer> getBackLaneType() {
            return mBackLaneType;
        }

        public void setBackLaneType(final List<Integer> backLaneType) {
            this.mBackLaneType = backLaneType;
        }

        @NonNull
        @Override
        public String toString() {
            return "CrossInfoDataBean{" +
                    "mHasTrafficLight=" + mHasTrafficLight +
                    ", mLaneNum=" + mLaneNum +
                    ", mTrafficLightPosition=" + mTrafficLightPosition +
                    ", mTimestamp=" + mTimestamp +
                    ", mSegmentIndex=" + mSegmentIndex +
                    ", mLinkIndex=" + mLinkIndex +
                    ", mHighLightLaneTypes=" + mHighLightLaneTypes +
                    ", mHighLightLanes=" + mHighLightLanes +
                    ", mLaneTypes=" + mLaneTypes +
                    ", mFrontLaneType=" + mFrontLaneType +
                    ", mBackLaneType=" + mBackLaneType +
                    '}';
        }
    }

    @Setter
    @Getter
    public static class GuidePointInfoBean {
        /**
         * nextGuideDist : 51
         * nextGuideType : 2
         */

        private int mNextGuideDist;
        private int mNextGuideType;

        public int getNextGuideType() {
            return mNextGuideType;
        }

        public void setNextGuideType(final int nextGuideType) {
            this.mNextGuideType = nextGuideType;
        }

        public int getNextGuideDist() {
            return mNextGuideDist;
        }

        public void setNextGuideDist(final int nextGuideDist) {
            this.mNextGuideDist = nextGuideDist;
        }

        @NonNull
        @Override
        public String toString() {
            return "GuidePointInfoBean{" +
                    "mNextGuideDist=" + mNextGuideDist +
                    ", mNextGuideType=" + mNextGuideType +
                    '}';
        }
    }

    @Setter
    @Getter
    public static class IntervalCameraDataBean {
        /**
         * intervalCameraEndPointDist : 0
         * intervalCameraSpeedValue : 0
         * intervalCameraStartPointDist : 0
         */

        private int mIntervalCameraEndPointDist;
        private int mIntervalCameraSpeedValue;
        private int mIntervalCameraStartPointDist;

        public int getIntervalCameraEndPointDist() {
            return mIntervalCameraEndPointDist;
        }

        public void setIntervalCameraEndPointDist(final int intervalCameraEndPointDist) {
            this.mIntervalCameraEndPointDist = intervalCameraEndPointDist;
        }

        public int getIntervalCameraSpeedValue() {
            return mIntervalCameraSpeedValue;
        }

        public void setIntervalCameraSpeedValue(final int intervalCameraSpeedValue) {
            this.mIntervalCameraSpeedValue = intervalCameraSpeedValue;
        }

        public int getIntervalCameraStartPointDist() {
            return mIntervalCameraStartPointDist;
        }

        public void setIntervalCameraStartPointDist(final int intervalCameraStartPointDist) {
            this.mIntervalCameraStartPointDist = intervalCameraStartPointDist;
        }

        @NonNull
        @Override
        public String toString() {
            return "IntervalCameraDataBean{" +
                    "mIntervalCameraEndPointDist=" + mIntervalCameraEndPointDist +
                    ", mIntervalCameraSpeedValue=" + mIntervalCameraSpeedValue +
                    ", mIntervalCameraStartPointDist=" + mIntervalCameraStartPointDist +
                    '}';
        }
    }

    @Setter
    @Getter
    public static class LimitCameraDataBean {
        /**
         * spdLmtEleEyeDist : 0
         * spdLmtEleEyeSpeedValue : 0
         */

        private int mSpdLmtEleEyeDist;
        private int mSpdLmtEleEyeSpeedValue;

        public int getSpdLmtEleEyeDist() {
            return mSpdLmtEleEyeDist;
        }

        public void setSpdLmtEleEyeDist(final int spdLmtEleEyeDist) {
            this.mSpdLmtEleEyeDist = spdLmtEleEyeDist;
        }

        public int getSpdLmtEleEyeSpeedValue() {
            return mSpdLmtEleEyeSpeedValue;
        }

        public void setSpdLmtEleEyeSpeedValue(final int spdLmtEleEyeSpeedValue) {
            this.mSpdLmtEleEyeSpeedValue = spdLmtEleEyeSpeedValue;
        }

        @NonNull
        @Override
        public String toString() {
            return "LimitCameraDataBean{" +
                    "mSpdLmtEleEyeDist=" + mSpdLmtEleEyeDist +
                    ", mSpdLmtEleEyeSpeedValue=" + mSpdLmtEleEyeSpeedValue +
                    '}';
        }
    }

    @Setter
    @Getter
    public static class TunnelInfoBean {
        /**
         * tunnelDist : 65535
         * tunnelLength : 0
         */

        private int mTunnelDist;
        private int mTunnelLength;

        public int getTunnelDist() {
            return mTunnelDist;
        }

        public void setTunnelDist(final int tunnelDist) {
            this.mTunnelDist = tunnelDist;
        }

        public int getTunnelLength() {
            return mTunnelLength;
        }

        public void setTunnelLength(final int tunnelLength) {
            this.mTunnelLength = tunnelLength;
        }

        @NonNull
        @Override
        public String toString() {
            return "TunnelInfoBean{" +
                    "mTunnelDist=" + mTunnelDist +
                    ", mTunnelLength=" + mTunnelLength +
                    '}';
        }
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

        private int mCurPathID;
        private int mCurrentSpeedLimit;
        private int mDistToDestination;
        private double mLocationLatitude;
        private int mLocationLinkIndex;
        private int mLocationLinkOffset;
        private double mLocationLongitude;
        private int mMainSideRots;
        private int mNaviStatus;
        private int mRoadClass;
        private int mRoadOwnership;
        private String mTtsText;
        private int mFormWay;
        private int mLinkType;

        public int getCurPathID() {
            return mCurPathID;
        }

        public void setCurPathID(final int curPathID) {
            this.mCurPathID = curPathID;
        }

        public int getCurrentSpeedLimit() {
            return mCurrentSpeedLimit;
        }

        public void setCurrentSpeedLimit(final int currentSpeedLimit) {
            this.mCurrentSpeedLimit = currentSpeedLimit;
        }

        public int getDistToDestination() {
            return mDistToDestination;
        }

        public void setDistToDestination(final int distToDestination) {
            this.mDistToDestination = distToDestination;
        }

        public double getLocationLatitude() {
            return mLocationLatitude;
        }

        public void setLocationLatitude(final double locationLatitude) {
            this.mLocationLatitude = locationLatitude;
        }

        public int getLocationLinkIndex() {
            return mLocationLinkIndex;
        }

        public void setLocationLinkIndex(final int locationLinkIndex) {
            this.mLocationLinkIndex = locationLinkIndex;
        }

        public int getLocationLinkOffset() {
            return mLocationLinkOffset;
        }

        public void setLocationLinkOffset(final int locationLinkOffset) {
            this.mLocationLinkOffset = locationLinkOffset;
        }

        public double getLocationLongitude() {
            return mLocationLongitude;
        }

        public void setLocationLongitude(final double locationLongitude) {
            this.mLocationLongitude = locationLongitude;
        }

        public int getMainSideRots() {
            return mMainSideRots;
        }

        public void setMainSideRots(final int mainSideRots) {
            this.mMainSideRots = mainSideRots;
        }

        public int getNaviStatus() {
            return mNaviStatus;
        }

        public void setNaviStatus(final int naviStatus) {
            this.mNaviStatus = naviStatus;
        }

        public int getRoadClass() {
            return mRoadClass;
        }

        public void setRoadClass(final int roadClass) {
            this.mRoadClass = roadClass;
        }

        public int getRoadOwnership() {
            return mRoadOwnership;
        }

        public void setRoadOwnership(final int roadOwnership) {
            this.mRoadOwnership = roadOwnership;
        }

        public String getTtsText() {
            return mTtsText;
        }

        public void setTtsText(final String ttsText) {
            this.mTtsText = ttsText;
        }

        public int getFormWay() {
            return mFormWay;
        }

        public void setFormWay(final int formWay) {
            this.mFormWay = formWay;
        }

        public int getLinkType() {
            return mLinkType;
        }

        public void setLinkType(final int linkType) {
            this.mLinkType = linkType;
        }

        @NonNull
        @Override
        public String toString() {
            return "VehiclePositionBean{" +
                    "mCurPathID=" + mCurPathID +
                    ", mCurrentSpeedLimit=" + mCurrentSpeedLimit +
                    ", mDistToDestination=" + mDistToDestination +
                    ", mLocationLatitude=" + mLocationLatitude +
                    ", mLocationLinkIndex=" + mLocationLinkIndex +
                    ", mLocationLinkOffset=" + mLocationLinkOffset +
                    ", mLocationLongitude=" + mLocationLongitude +
                    ", mMainSideRots=" + mMainSideRots +
                    ", mNaviStatus=" + mNaviStatus +
                    ", mRoadClass=" + mRoadClass +
                    ", mRoadOwnership=" + mRoadOwnership +
                    ", mTtsText='" + mTtsText + '\'' +
                    ", mFormWay=" + mFormWay +
                    ", mLinkType=" + mLinkType +
                    '}';
        }
    }

    @Setter
    @Getter
    public static class WarningFacilityBean {
        /**
         * boardSignDist : 0
         * boardSignType : 0
         * limitSpeed : 0
         */

        private int mBoardSignDist;
        private int mBoardSignType;
        private int mLimitSpeed;

        public int getBoardSignDist() {
            return mBoardSignDist;
        }

        public void setBoardSignDist(final int boardSignDist) {
            this.mBoardSignDist = boardSignDist;
        }

        public int getBoardSignType() {
            return mBoardSignType;
        }

        public void setBoardSignType(final int boardSignType) {
            this.mBoardSignType = boardSignType;
        }

        public int getLimitSpeed() {
            return mLimitSpeed;
        }

        public void setLimitSpeed(final int limitSpeed) {
            this.mLimitSpeed = limitSpeed;
        }

        @NonNull
        @Override
        public String toString() {
            return "WarningFacilityBean{" +
                    "mBoardSignDist=" + mBoardSignDist +
                    ", mBoardSignType=" + mBoardSignType +
                    ", mLimitSpeed=" + mLimitSpeed +
                    '}';
        }
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

        private int mLaneNum;
        private int mTimestamp;
        private int mSegmentIndex;
        private int mLinkIndex;
        private List<Integer> mHighLightLaneTypes;
        private List<Integer> mLaneTypes;
        private List<Integer> mFrontLaneType;
        private List<Integer> mBackLaneType;

        public int getLaneNum() {
            return mLaneNum;
        }

        public void setLaneNum(final int laneNum) {
            this.mLaneNum = laneNum;
        }

        public int getTimestamp() {
            return mTimestamp;
        }

        public void setTimestamp(final int timestamp) {
            this.mTimestamp = timestamp;
        }

        public int getSegmentIndex() {
            return mSegmentIndex;
        }

        public void setSegmentIndex(final int segmentIndex) {
            this.mSegmentIndex = segmentIndex;
        }

        public int getLinkIndex() {
            return mLinkIndex;
        }

        public void setLinkIndex(final int linkIndex) {
            this.mLinkIndex = linkIndex;
        }

        public List<Integer> getHighLightLaneTypes() {
            return mHighLightLaneTypes;
        }

        public void setHighLightLaneTypes(final List<Integer> highLightLaneTypes) {
            this.mHighLightLaneTypes = highLightLaneTypes;
        }

        public List<Integer> getLaneTypes() {
            return mLaneTypes;
        }

        public void setLaneTypes(final List<Integer> laneTypes) {
            this.mLaneTypes = laneTypes;
        }

        public List<Integer> getFrontLaneType() {
            return mFrontLaneType;
        }

        public void setFrontLaneType(final List<Integer> frontLaneType) {
            this.mFrontLaneType = frontLaneType;
        }

        public List<Integer> getBackLaneType() {
            return mBackLaneType;
        }

        public void setBackLaneType(final List<Integer> backLaneType) {
            this.mBackLaneType = backLaneType;
        }

        @NonNull
        @Override
        public String toString() {
            return "AheadIntersectionsBean{" +
                    "mLaneNum=" + mLaneNum +
                    ", mTimestamp=" + mTimestamp +
                    ", mSegmentIndex=" + mSegmentIndex +
                    ", mLinkIndex=" + mLinkIndex +
                    ", mHighLightLaneTypes=" + mHighLightLaneTypes +
                    ", mLaneTypes=" + mLaneTypes +
                    ", mFrontLaneType=" + mFrontLaneType +
                    ", mBackLaneType=" + mBackLaneType +
                    '}';
        }
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

        private int mDistance;
        private PositionBean mPosition = new PositionBean();
        private int mRoadClass;
        private int mSegmentIndex;

        public int getDistance() {
            return mDistance;
        }

        public void setDistance(final int distance) {
            this.mDistance = distance;
        }

        public PositionBean getPosition() {
            return mPosition;
        }

        public void setPosition(final PositionBean position) {
            this.mPosition = position;
        }

        public int getRoadClass() {
            return mRoadClass;
        }

        public void setRoadClass(final int roadClass) {
            this.mRoadClass = roadClass;
        }

        public int getSegmentIndex() {
            return mSegmentIndex;
        }

        public void setSegmentIndex(final int segmentIndex) {
            this.mSegmentIndex = segmentIndex;
        }

        @NonNull
        @Override
        public String toString() {
            return "MixForksBean{" +
                    "mDistance=" + mDistance +
                    ", mPosition=" + mPosition +
                    ", mRoadClass=" + mRoadClass +
                    ", mSegmentIndex=" + mSegmentIndex +
                    '}';
        }
    }

    @Setter
    @Getter
    public static class PositionBean {
        /**
         * x : 31.11555557
         * y : 121.60491889
         */

        private double mX;
        private double mY;

        public double getX() {
            return mX;
        }

        public void setX(final double x) {
            this.mX = x;
        }

        public double getY() {
            return mY;
        }

        public void setY(final double y) {
            this.mY = y;
        }

        @NonNull
        @Override
        public String toString() {
            return "PositionBean{" +
                    "mX=" + mX +
                    ", mY=" + mY +
                    '}';
        }
    }

    @Setter
    @Getter
    public static class EndParkingInfo {
        private PositionBean mParkingEnter = new PositionBean();
        private PositionBean mParkingExit = new PositionBean();

        public EndParkingInfo() {
        }

        public EndParkingInfo(final PositionBean parkingEnter, final PositionBean parkingExit) {
            this.mParkingEnter = parkingEnter;
            this.mParkingExit = parkingExit;
        }

        public EndParkingInfo(final double enterX, final double enterY, final double exitX,
                              final double exitY) {
            mParkingEnter.mX = enterX;
            mParkingEnter.mY = enterY;
            mParkingExit.mX = exitX;
            mParkingExit.mY = exitY;
        }

        /**
         * @param enterX enterX
         * @param enterY enterY
         */
        public void setMParkingEnter(final double enterX, final double enterY) {
            mParkingEnter.mX = enterX;
            mParkingEnter.mY = enterY;
        }

        /**
         * @param exitX exitX
         * @param exitY exitY
         */
        public void setMParkingExit(final double exitX, final double exitY) {
            mParkingExit.mX = exitX;
            mParkingExit.mY = exitY;
        }
    }

    public CrossInfoDataBean getCrossInfoData() {
        return mCrossInfoData;
    }

    public void setCrossInfoData(final CrossInfoDataBean crossInfoData) {
        this.mCrossInfoData = crossInfoData;
    }

    public GuidePointInfoBean getGuidePointInfo() {
        return mGuidePointInfo;
    }

    public void setGuidePointInfo(final GuidePointInfoBean guidePointInfo) {
        this.mGuidePointInfo = guidePointInfo;
    }

    public int getHasTidalLane() {
        return mHasTidalLane;
    }

    public void setHasTidalLane(final int hasTidalLane) {
        this.mHasTidalLane = hasTidalLane;
    }

    public IntervalCameraDataBean getIntervalCameraData() {
        return mIntervalCameraData;
    }

    public void setIntervalCameraData(final IntervalCameraDataBean intervalCameraData) {
        this.mIntervalCameraData = intervalCameraData;
    }

    public int getIsServiceAreaRoad() {
        return mIsServiceAreaRoad;
    }

    public void setIsServiceAreaRoad(final int isServiceAreaRoad) {
        this.mIsServiceAreaRoad = isServiceAreaRoad;
    }

    public LimitCameraDataBean getLimitCameraData() {
        return mLimitCameraData;
    }

    public void setLimitCameraData(final LimitCameraDataBean limitCameraData) {
        this.mLimitCameraData = limitCameraData;
    }

    public int getRampDist() {
        return mRampDist;
    }

    public void setRampDist(final int rampDist) {
        this.mRampDist = rampDist;
    }

    public int getTollStationDist() {
        return mTollStationDist;
    }

    public void setTollStationDist(final int tollStationDist) {
        this.mTollStationDist = tollStationDist;
    }

    public TunnelInfoBean getTunnelInfo() {
        return mTunnelInfo;
    }

    public void setTunnelInfo(final TunnelInfoBean tunnelInfo) {
        this.mTunnelInfo = tunnelInfo;
    }

    public VehiclePositionBean getVehiclePosition() {
        return mVehiclePosition;
    }

    public void setVehiclePosition(final VehiclePositionBean vehiclePosition) {
        this.mVehiclePosition = vehiclePosition;
    }

    public WarningFacilityBean getWarningFacility() {
        return mWarningFacility;
    }

    public void setWarningFacility(final WarningFacilityBean warningFacility) {
        this.mWarningFacility = warningFacility;
    }

    public List<AheadIntersectionsBean> getAheadIntersections() {
        return mAheadIntersections;
    }

    public void setAheadIntersections(final List<AheadIntersectionsBean> aheadIntersections) {
        this.mAheadIntersections = aheadIntersections;
    }

    public List<MixForksBean> getMixForks() {
        return mMixForks;
    }

    public void setMixForks(final List<MixForksBean> mixForks) {
        this.mMixForks = mixForks;
    }

    public EndParkingInfo getEndParkingInfo() {
        return mEndParkingInfo;
    }

    public void setEndParkingInfo(final EndParkingInfo endParkingInfo) {
        this.mEndParkingInfo = endParkingInfo;
    }

    @NonNull
    @Override
    public String toString() {
        return "L2NaviBean{" +
                "mCrossInfoData=" + mCrossInfoData +
                ", mGuidePointInfo=" + mGuidePointInfo +
                ", mHasTidalLane=" + mHasTidalLane +
                ", mIntervalCameraData=" + mIntervalCameraData +
                ", mIsServiceAreaRoad=" + mIsServiceAreaRoad +
                ", mLimitCameraData=" + mLimitCameraData +
                ", mRampDist=" + mRampDist +
                ", mTollStationDist=" + mTollStationDist +
                ", mTunnelInfo=" + mTunnelInfo +
                ", mVehiclePosition=" + mVehiclePosition +
                ", mWarningFacility=" + mWarningFacility +
                ", mAheadIntersections=" + mAheadIntersections +
                ", mMixForks=" + mMixForks +
                ", mEndParkingInfo=" + mEndParkingInfo +
                '}';
    }
}

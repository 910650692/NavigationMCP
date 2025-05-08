package com.fy.navi.service.define.route;

import com.google.gson.annotations.SerializedName;

import java.util.List;

import lombok.Data;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@NoArgsConstructor
@Data
@Getter
@Setter
public class RouteL2Data {

    @SerializedName("sdkVersion")
    private String mSdkVersion;
    @SerializedName("engineVersion")
    private String mEngineVersion;
    @SerializedName("guideGroups")
    private List<GuideGroupsDTO> mGuideGroups;
    @SerializedName("linkCnt")
    private Integer mLinkCnt;
    @SerializedName("links")
    private List<LinksDTO> mLinks;
    @SerializedName("pathID")
    private Integer mPathID;
    @SerializedName("pntCnt")
    private Integer mPntCnt;
    @SerializedName("pnts")
    private List<PntsDTO> mPnts;
    @SerializedName("restTollGateInfos")
    private List<RestTollGateInfosDTO> mRestTollGateInfos;
    @SerializedName("trafficLights")
    private List<TrafficLightsDTO> mTrafficLights;
    @SerializedName("viaRoads")
    private List<ViaRoadsDTO> mViaRoads;
    @SerializedName("endPoi")
    private EndPoiDTO mEndPoi;

    @NoArgsConstructor
    @Data
    public static class EndPoiDTO {
        @SerializedName("id")
        private String mId;
        @SerializedName("Name")
        private String mName;
        @SerializedName("Type")
        private Integer mType;
        @SerializedName("EntranceList")
        private List<EntranceListDTO> mEntranceList;
        @SerializedName("ExitList")
        private List<ExitListDTO> mExitList;
        @SerializedName("ParkingInfoList")
        private List<ParkingInfoListDTO> mParkingInfoList;

        @NoArgsConstructor
        @Data
        public static class EntranceListDTO {
            @SerializedName("x")
            private Double mX;
            @SerializedName("y")
            private Double mY;
        }

        @NoArgsConstructor
        @Data
        public static class ExitListDTO {
            @SerializedName("x")
            private Double mX;
            @SerializedName("y")
            private Double mY;
        }

        @NoArgsConstructor
        @Data
        public static class ParkingInfoListDTO {
            @SerializedName("id")
            private String mId;
            @SerializedName("Name")
            private String mName;
            @SerializedName("Type")
            private Integer mType;
            @SerializedName("EntranceList")
            private List<EntranceListDTO> mEntranceList;
            @SerializedName("ExitList")
            private List<ExitListDTO> mExitList;

            @NoArgsConstructor
            @Data
            public static class EntranceListDTO {
                @SerializedName("x")
                private Double mX;
                @SerializedName("y")
                private Double mY;
            }

            @NoArgsConstructor
            @Data
            public static class ExitListDTO {
                @SerializedName("x")
                private Double mX;
                @SerializedName("y")
                private Double mY;
            }
        }
    }

    @NoArgsConstructor
    @Data
    public static class GuideGroupsDTO {
        @SerializedName("groupEnterCoord")
        private GroupEnterCoordDTO mGroupEnterCoord;
        @SerializedName("groupIconType")
        private Integer mGroupIconType;
        @SerializedName("groupLen")
        private Integer mGroupLen;
        @SerializedName("groupName")
        private String mGroupName;
        @SerializedName("groupTime")
        private Integer mGroupTime;
        @SerializedName("groupTrafficLightsCount")
        private Integer mGroupTrafficLightsCount;
        @SerializedName("segments")
        private List<SegmentsDTO> mSegments;

        @NoArgsConstructor
        @Data
        public static class GroupEnterCoordDTO {
            @SerializedName("map")
            private MapDTO mMap;

            @NoArgsConstructor
            @Data
            public static class MapDTO {
                @SerializedName("x")
                private Double mX;
                @SerializedName("y")
                private Double mY;
            }
        }

        @NoArgsConstructor
        @Data
        public static class SegmentsDTO {
            @SerializedName("crntSegmLinkCnt")
            private Integer mCrntSegmLinkCnt;
            @SerializedName("description")
            private String mDescription;
            @SerializedName("isArriveWayPoint")
            private Boolean mIsArriveWayPoint;
            @SerializedName("linkBegIdx")
            private Integer mLinkBegIdx;
            @SerializedName("navigationAssistAction")
            private Integer mNavigationAssistAction;
            @SerializedName("navigationLen")
            private Integer mNavigationLen;
            @SerializedName("navigationMainAction")
            private Integer mNavigationMainAction;
            @SerializedName("navigationNextRoadName")
            private String mNavigationNextRoadName;
            @SerializedName("trafficLightNum")
            private Integer mTrafficLightNum;
            @SerializedName("travelTime")
            private Integer mTravelTime;
        }
    }

    @NoArgsConstructor
    @Data
    public static class LinksDTO {
        @SerializedName("formway")
        private Integer mFormway;
        @SerializedName("isToll")
        private Boolean mIsToll;
        @SerializedName("len")
        private Integer mLen;
        @SerializedName("linkID")
        private Integer mLinkID;
        @SerializedName("linktype")
        private Integer mLinktype;
        @SerializedName("pntBegIdx")
        private Integer mPntBegIdx;
        @SerializedName("pntCnt")
        private Integer mPntCnt;
        @SerializedName("roadclass")
        private Integer mRoadclass;
        @SerializedName("roadname")
        private String mRoadname;
        @SerializedName("urid")
        private Integer mUrid;
        @SerializedName("adminCode")
        private Integer mAdminCode;
        @SerializedName("HasMixFork")
        private Boolean mHasMixFork;
        @SerializedName("HasTrafficLight")
        private Boolean mHasTrafficLight;
        @SerializedName("HasMultiOut")
        private Boolean mHasMultiOut;
        @SerializedName("mainAction")
        private Integer mMainAction;
        @SerializedName("assistantAction")
        private Integer mAssistantAction;
        @SerializedName("hasParallel")
        private Boolean mHasParallel;
        @SerializedName("direction")
        private Integer mDirection;
    }

    @NoArgsConstructor
    @Data
    public static class PntsDTO {
        @SerializedName("x")
        private Double mX;
        @SerializedName("y")
        private Double mY;
    }

    @NoArgsConstructor
    @Data
    public static class RestTollGateInfosDTO {
        @SerializedName("pos")
        private PosDTO mPos;
        @SerializedName("remainDist")
        private Integer mRemainDist;
        @SerializedName("remainTime")
        private Integer mRemainTime;
        @SerializedName("tollGateName")
        private String mTollGateName;

        @NoArgsConstructor
        @Data
        public static class PosDTO {
            @SerializedName("x")
            private Double mX;
            @SerializedName("y")
            private Double mY;
        }
    }

    @NoArgsConstructor
    @Data
    public static class TrafficLightsDTO {
        @SerializedName("x")
        private Double mX;
        @SerializedName("y")
        private Double mY;
    }

    @NoArgsConstructor
    @Data
    public static class ViaRoadsDTO {
        @SerializedName("roadName")
        private String mRoadName;
        @SerializedName("minLaneNum")
        private Integer mMinLaneNum;
        @SerializedName("maxLaneNum")
        private Integer mMaxLaneNum;
        @SerializedName("minSpeedLimit")
        private Integer mMinSpeedLimit;
        @SerializedName("maxSpeedLimit")
        private Integer mMaxSpeedLimit;
        @SerializedName("length")
        private Integer mLength;
        @SerializedName("roadClass")
        private Integer mRoadClass;
        @SerializedName("coordinate")
        private CoordinateDTO mCoordinate;

        @NoArgsConstructor
        @Data
        public static class CoordinateDTO {
            @SerializedName("x")
            private Double mX;
            @SerializedName("y")
            private Double mY;
        }
    }
}

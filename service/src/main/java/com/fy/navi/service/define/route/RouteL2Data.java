package com.fy.navi.service.define.route;

import com.google.gson.annotations.SerializedName;

import java.util.List;

import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@Data
public class RouteL2Data {

    @SerializedName("sdkVersion")
    public String sdkVersion;
    @SerializedName("engineVersion")
    public String engineVersion;
    @SerializedName("guideGroups")
    public List<GuideGroupsDTO> guideGroups;
    @SerializedName("linkCnt")
    public Integer linkCnt;
    @SerializedName("links")
    public List<LinksDTO> links;
    @SerializedName("pathID")
    public Integer pathID;
    @SerializedName("pntCnt")
    public Integer pntCnt;
    @SerializedName("pnts")
    public List<PntsDTO> pnts;
    @SerializedName("restTollGateInfos")
    public List<RestTollGateInfosDTO> restTollGateInfos;
    @SerializedName("trafficLights")
    public List<TrafficLightsDTO> trafficLights;
    @SerializedName("viaRoads")
    public List<ViaRoadsDTO> viaRoads;
    @SerializedName("endPoi")
    public EndPoiDTO endPoi;

    @NoArgsConstructor
    @Data
    public static class EndPoiDTO {
        @SerializedName("id")
        public String id;
        @SerializedName("Name")
        public String name;
        @SerializedName("Type")
        public Integer type;
        @SerializedName("EntranceList")
        public List<EntranceListDTO> entranceList;
        @SerializedName("ExitList")
        public List<ExitListDTO> exitList;
        @SerializedName("ParkingInfoList")
        public List<ParkingInfoListDTO> parkingInfoList;

        @NoArgsConstructor
        @Data
        public static class EntranceListDTO {
            @SerializedName("x")
            public Double x;
            @SerializedName("y")
            public Double y;
        }

        @NoArgsConstructor
        @Data
        public static class ExitListDTO {
            @SerializedName("x")
            public Double x;
            @SerializedName("y")
            public Double y;
        }

        @NoArgsConstructor
        @Data
        public static class ParkingInfoListDTO {
            @SerializedName("id")
            public String id;
            @SerializedName("Name")
            public String name;
            @SerializedName("Type")
            public Integer type;
            @SerializedName("EntranceList")
            public List<EntranceListDTO> entranceList;
            @SerializedName("ExitList")
            public List<ExitListDTO> exitList;

            @NoArgsConstructor
            @Data
            public static class EntranceListDTO {
                @SerializedName("x")
                public Double x;
                @SerializedName("y")
                public Double y;
            }

            @NoArgsConstructor
            @Data
            public static class ExitListDTO {
                @SerializedName("x")
                public Double x;
                @SerializedName("y")
                public Double y;
            }
        }
    }

    @NoArgsConstructor
    @Data
    public static class GuideGroupsDTO {
        @SerializedName("groupEnterCoord")
        public GroupEnterCoordDTO groupEnterCoord;
        @SerializedName("groupIconType")
        public Integer groupIconType;
        @SerializedName("groupLen")
        public Integer groupLen;
        @SerializedName("groupName")
        public String groupName;
        @SerializedName("groupTime")
        public Integer groupTime;
        @SerializedName("groupTrafficLightsCount")
        public Integer groupTrafficLightsCount;
        @SerializedName("segments")
        public List<SegmentsDTO> segments;

        @NoArgsConstructor
        @Data
        public static class GroupEnterCoordDTO {
            @SerializedName("map")
            public MapDTO map;

            @NoArgsConstructor
            @Data
            public static class MapDTO {
                @SerializedName("x")
                public Double x;
                @SerializedName("y")
                public Double y;
            }
        }

        @NoArgsConstructor
        @Data
        public static class SegmentsDTO {
            @SerializedName("crntSegmLinkCnt")
            public Integer crntSegmLinkCnt;
            @SerializedName("description")
            public String description;
            @SerializedName("isArriveWayPoint")
            public Boolean isArriveWayPoint;
            @SerializedName("linkBegIdx")
            public Integer linkBegIdx;
            @SerializedName("navigationAssistAction")
            public Integer navigationAssistAction;
            @SerializedName("navigationLen")
            public Integer navigationLen;
            @SerializedName("navigationMainAction")
            public Integer navigationMainAction;
            @SerializedName("navigationNextRoadName")
            public String navigationNextRoadName;
            @SerializedName("trafficLightNum")
            public Integer trafficLightNum;
            @SerializedName("travelTime")
            public Integer travelTime;
        }
    }

    @NoArgsConstructor
    @Data
    public static class LinksDTO {
        @SerializedName("formway")
        public Integer formway;
        @SerializedName("isToll")
        public Boolean isToll;
        @SerializedName("len")
        public Integer len;
        @SerializedName("linkID")
        public Integer linkID;
        @SerializedName("linktype")
        public Integer linktype;
        @SerializedName("pntBegIdx")
        public Integer pntBegIdx;
        @SerializedName("pntCnt")
        public Integer pntCnt;
        @SerializedName("roadclass")
        public Integer roadclass;
        @SerializedName("roadname")
        public String roadname;
        @SerializedName("urid")
        public Integer urid;
        @SerializedName("adminCode")
        public Integer adminCode;
        @SerializedName("HasMixFork")
        public Boolean HasMixFork;
        @SerializedName("HasTrafficLight")
        public Boolean HasTrafficLight;
        @SerializedName("HasMultiOut")
        public Boolean HasMultiOut;
        @SerializedName("mainAction")
        public Integer mainAction;
        @SerializedName("assistantAction")
        public Integer assistantAction;
        @SerializedName("hasParallel")
        public Boolean hasParallel;
        @SerializedName("direction")
        public Integer direction;
    }

    @NoArgsConstructor
    @Data
    public static class PntsDTO {
        @SerializedName("x")
        public Double x;
        @SerializedName("y")
        public Double y;
    }

    @NoArgsConstructor
    @Data
    public static class RestTollGateInfosDTO {
        @SerializedName("pos")
        public PosDTO pos;
        @SerializedName("remainDist")
        public Integer remainDist;
        @SerializedName("remainTime")
        public Integer remainTime;
        @SerializedName("tollGateName")
        public String tollGateName;

        @NoArgsConstructor
        @Data
        public static class PosDTO {
            @SerializedName("x")
            public Double x;
            @SerializedName("y")
            public Double y;
        }
    }

    @NoArgsConstructor
    @Data
    public static class TrafficLightsDTO {
        @SerializedName("x")
        public Double x;
        @SerializedName("y")
        public Double y;
    }

    @NoArgsConstructor
    @Data
    public static class ViaRoadsDTO {
        @SerializedName("roadName")
        public String roadName;
        @SerializedName("minLaneNum")
        public Integer minLaneNum;
        @SerializedName("maxLaneNum")
        public Integer maxLaneNum;
        @SerializedName("minSpeedLimit")
        public Integer minSpeedLimit;
        @SerializedName("maxSpeedLimit")
        public Integer maxSpeedLimit;
        @SerializedName("length")
        public Integer length;
        @SerializedName("roadClass")
        public Integer roadClass;
        @SerializedName("coordinate")
        public CoordinateDTO coordinate;

        @NoArgsConstructor
        @Data
        public static class CoordinateDTO {
            @SerializedName("x")
            public Double x;
            @SerializedName("y")
            public Double y;
        }
    }
}

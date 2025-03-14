package com.fy.navi.service.define.navi;

import com.fy.navi.service.define.bean.GeoPoint;

import java.util.ArrayList;

public class NaviTmcInfo {
    public NaviLightBarDetail lightBarDetail;
    public ArrayList<NaviLightBarInfo> lightBarInfo;

    public static class NaviLightBarInfo {
        public ArrayList<NaviLightBarItem> itemList;
        public long pathID;
    }

    public static class NaviLightBarItem {
        public short statusFlag;
        public int status;
        public int fineStatus;
        public int length;
        public long timeOfSeconds;
        public int startSegmentIdx;
        public int startLinkIdx;
        public long startLinkStatus;
        public long startLinkFineStatus;
        public int endSegmentIdx;
        public int endLinkIndex;
        public long endLinkStatus;
        public long endLinkFineStatus;
        public NaviTrafficItem startTrafficItem;
        public NaviTrafficItem start3dTrafficItem;
        public NaviTrafficItem endTrafficItem;
        public NaviTrafficItem end3dTrafficItem;
    }

    public static class NaviTrafficItem {
        public long length;
        public int travelTime;
        public int ratio;
        public int startIndex;
        public int endIndex;
        public short status;
        public int fineStatus;
        public short speed;
        public short credibility;
        public short reverse;
        public GeoPoint startPnt;
        public GeoPoint endPnt;
    }

    public static class NaviLightBarDetail {
        public long pathID;
        public int totalDistance;
        public int restDistance;
        public int finishDistance;
        public ArrayList<NaviTmcInfoData> tmcInfoData;
    }

    public static class NaviTmcInfoData {
        public int number;
        public int status;
        public int distance;
        public float percent;
        public int travelTime;
    }
}

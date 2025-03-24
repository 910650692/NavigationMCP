package com.fy.navi.service.define.navi;

import androidx.annotation.NonNull;

import com.fy.navi.service.define.bean.GeoPoint;

import java.util.ArrayList;

public class NaviTmcInfo {
    private NaviLightBarDetail mLightBarDetail;
    private ArrayList<NaviLightBarInfo> mLightBarInfo;

    public static class NaviLightBarInfo {
        private ArrayList<NaviLightBarItem> mItemList;
        private long mPathID;

        public ArrayList<NaviLightBarItem> getItemList() {
            return mItemList;
        }

        public void setItemList(final ArrayList<NaviLightBarItem> itemList) {
            this.mItemList = itemList;
        }

        public long getPathID() {
            return mPathID;
        }

        public void setPathID(final long pathID) {
            this.mPathID = pathID;
        }

        @NonNull
        @Override
        public String toString() {
            return "NaviLightBarInfo{" +
                    "itemList=" + mItemList +
                    ", pathID=" + mPathID +
                    '}';
        }
    }

    public static class NaviLightBarItem {
        private short mStatusFlag;
        private int mStatus;
        private int mFineStatus;
        private int mLength;
        private long mTimeOfSeconds;
        private int mStartSegmentIdx;
        private int mStartLinkIdx;
        private long mStartLinkStatus;
        private long mStartLinkFineStatus;
        private int mEndSegmentIdx;
        private int mEndLinkIndex;
        private long mEndLinkStatus;
        private long mEndLinkFineStatus;
        private NaviTrafficItem mStartTrafficItem;
        private NaviTrafficItem mStart3dTrafficItem;
        private NaviTrafficItem mEndTrafficItem;
        private NaviTrafficItem mEnd3dTrafficItem;

        public short getStatusFlag() {
            return mStatusFlag;
        }

        public void setStatusFlag(final short statusFlag) {
            this.mStatusFlag = statusFlag;
        }

        public int getStatus() {
            return mStatus;
        }

        public void setStatus(final int status) {
            this.mStatus = status;
        }

        public int getFineStatus() {
            return mFineStatus;
        }

        public void setFineStatus(final int fineStatus) {
            this.mFineStatus = fineStatus;
        }

        public int getLength() {
            return mLength;
        }

        public void setLength(final int length) {
            this.mLength = length;
        }

        public long getTimeOfSeconds() {
            return mTimeOfSeconds;
        }

        public void setTimeOfSeconds(final long timeOfSeconds) {
            this.mTimeOfSeconds = timeOfSeconds;
        }

        public int getStartSegmentIdx() {
            return mStartSegmentIdx;
        }

        public void setStartSegmentIdx(final int startSegmentIdx) {
            this.mStartSegmentIdx = startSegmentIdx;
        }

        public int getStartLinkIdx() {
            return mStartLinkIdx;
        }

        public void setStartLinkIdx(final int startLinkIdx) {
            this.mStartLinkIdx = startLinkIdx;
        }

        public long getStartLinkStatus() {
            return mStartLinkStatus;
        }

        public void setStartLinkStatus(final long startLinkStatus) {
            this.mStartLinkStatus = startLinkStatus;
        }

        public long getStartLinkFineStatus() {
            return mStartLinkFineStatus;
        }

        public void setStartLinkFineStatus(final long startLinkFineStatus) {
            this.mStartLinkFineStatus = startLinkFineStatus;
        }

        public int getEndSegmentIdx() {
            return mEndSegmentIdx;
        }

        public void setEndSegmentIdx(final int endSegmentIdx) {
            this.mEndSegmentIdx = endSegmentIdx;
        }

        public int getEndLinkIndex() {
            return mEndLinkIndex;
        }

        public void setEndLinkIndex(final int endLinkIndex) {
            this.mEndLinkIndex = endLinkIndex;
        }

        public long getEndLinkStatus() {
            return mEndLinkStatus;
        }

        public void setEndLinkStatus(final long endLinkStatus) {
            this.mEndLinkStatus = endLinkStatus;
        }

        public long getEndLinkFineStatus() {
            return mEndLinkFineStatus;
        }

        public void setEndLinkFineStatus(final long endLinkFineStatus) {
            this.mEndLinkFineStatus = endLinkFineStatus;
        }

        public NaviTrafficItem getStartTrafficItem() {
            return mStartTrafficItem;
        }

        public void setStartTrafficItem(final NaviTrafficItem startTrafficItem) {
            this.mStartTrafficItem = startTrafficItem;
        }

        public NaviTrafficItem getStart3dTrafficItem() {
            return mStart3dTrafficItem;
        }

        public void setStart3dTrafficItem(final NaviTrafficItem start3dTrafficItem) {
            this.mStart3dTrafficItem = start3dTrafficItem;
        }

        public NaviTrafficItem getEndTrafficItem() {
            return mEndTrafficItem;
        }

        public void setEndTrafficItem(final NaviTrafficItem endTrafficItem) {
            this.mEndTrafficItem = endTrafficItem;
        }

        public NaviTrafficItem getEnd3dTrafficItem() {
            return mEnd3dTrafficItem;
        }

        public void setEnd3dTrafficItem(final NaviTrafficItem end3dTrafficItem) {
            this.mEnd3dTrafficItem = end3dTrafficItem;
        }

        @NonNull
        @Override
        public String toString() {
            return "NaviLightBarItem{" +
                    "statusFlag=" + mStatusFlag +
                    ", status =" + mStatus +
                    ", fineStatus=" + mFineStatus +
                    ", length=" + mLength +
                    ", timeOfSeconds=" + mTimeOfSeconds +
                    ", startSegmentIdx=" + mStartSegmentIdx +
                    ", startLinkIdx=" + mStartLinkIdx +
                    ", startLinkStatus=" + mStartLinkStatus +
                    ", startLinkFineStatus=" + mStartLinkFineStatus +
                    ", endSegmentIdx=" + mEndSegmentIdx +
                    ", endLinkIndex=" + mEndLinkIndex +
                    ", endLinkStatus=" + mEndLinkStatus +
                    ", endLinkFineStatus=" + mEndLinkFineStatus +
                    ", startTrafficItem=" + mStartTrafficItem +
                    ", start3dTrafficItem=" + mStart3dTrafficItem +
                    ", endTrafficItem=" + mEndTrafficItem +
                    ", end3dTrafficItem=" + mEnd3dTrafficItem +
                    '}';
        }
    }

    public static class NaviTrafficItem {
        private long mLength;
        private int mTravelTime;
        private int mRatio;
        private int mStartIndex;
        private int mEndIndex;
        private short mStatus;
        private int mFineStatus;
        private short mSpeed;
        private short mCredibility;
        private short mReverse;
        private GeoPoint mStartPnt;
        private GeoPoint mEndPnt;

        public long getLength() {
            return mLength;
        }

        public void setLength(final long length) {
            this.mLength = length;
        }

        public int getTravelTime() {
            return mTravelTime;
        }

        public void setTravelTime(final int travelTime) {
            this.mTravelTime = travelTime;
        }

        public int getRatio() {
            return mRatio;
        }

        public void setRatio(final int ratio) {
            this.mRatio = ratio;
        }

        public int getStartIndex() {
            return mStartIndex;
        }

        public void setStartIndex(final int startIndex) {
            this.mStartIndex = startIndex;
        }

        public int getEndIndex() {
            return mEndIndex;
        }

        public void setEndIndex(final int endIndex) {
            this.mEndIndex = endIndex;
        }

        public short getStatus() {
            return mStatus;
        }

        public void setStatus(final short status) {
            this.mStatus = status;
        }

        public int getFineStatus() {
            return mFineStatus;
        }

        public void setFineStatus(final int fineStatus) {
            this.mFineStatus = fineStatus;
        }

        public short getSpeed() {
            return mSpeed;
        }

        public void setSpeed(final short speed) {
            this.mSpeed = speed;
        }

        public short getCredibility() {
            return mCredibility;
        }

        public void setCredibility(final short credibility) {
            this.mCredibility = credibility;
        }

        public short getReverse() {
            return mReverse;
        }

        public void setReverse(final short reverse) {
            this.mReverse = reverse;
        }

        public GeoPoint getStartPnt() {
            return mStartPnt;
        }

        public void setStartPnt(final GeoPoint startPnt) {
            this.mStartPnt = startPnt;
        }

        public GeoPoint getEndPnt() {
            return mEndPnt;
        }

        public void setEndPnt(final GeoPoint endPnt) {
            this.mEndPnt = endPnt;
        }

        @NonNull
        @Override
        public String toString() {
            return "NaviTrafficItem{" +
                    "length=" + mLength +
                    ", travelTime=" + mTravelTime +
                    ", ratio=" + mRatio +
                    ", startIndex=" + mStartIndex +
                    ", endIndex=" + mEndIndex +
                    ", status=" + mStatus +
                    ", fineStatus=" + mFineStatus +
                    ", speed=" + mSpeed +
                    ", credibility=" + mCredibility +
                    ", reverse=" + mReverse +
                    ", startPnt=" + mStartPnt +
                    ", endPnt=" + mEndPnt +
                    '}';
        }
    }

    public static class NaviLightBarDetail {
        private long mPathID;
        private int mTotalDistance;
        private int mRestDistance;
        private int mFinishDistance;
        private ArrayList<NaviTmcInfoData> mTmcInfoData;

        public ArrayList<NaviTmcInfoData> getTmcInfoData() {
            return mTmcInfoData;
        }

        public void setTmcInfoData(final ArrayList<NaviTmcInfoData> tmcInfoData) {
            this.mTmcInfoData = tmcInfoData;
        }

        public int getFinishDistance() {
            return mFinishDistance;
        }

        public void setFinishDistance(final int finishDistance) {
            this.mFinishDistance = finishDistance;
        }

        public int getRestDistance() {
            return mRestDistance;
        }

        public void setRestDistance(final int restDistance) {
            this.mRestDistance = restDistance;
        }

        public int getTotalDistance() {
            return mTotalDistance;
        }

        public void setTotalDistance(final int totalDistance) {
            this.mTotalDistance = totalDistance;
        }

        public long getPathID() {
            return mPathID;
        }

        public void setPathID(final long pathID) {
            this.mPathID = pathID;
        }

        @NonNull
        @Override
        public String toString() {
            return "NaviLightBarDetail{" +
                    "pathID=" + mPathID +
                    ", totalDistance=" + mTotalDistance +
                    ", restDistance=" + mRestDistance +
                    ", finishDistance=" + mFinishDistance +
                    ", tmcInfoData=" + mTmcInfoData +
                    '}';
        }
    }

    public static class NaviTmcInfoData {
        private int mNumber;
        private int mStatus;
        private int mDistance;
        private float mPercent;
        private int mTravelTime;

        public int getNumber() {
            return mNumber;
        }

        public void setNumber(final int number) {
            this.mNumber = number;
        }

        public int getStatus() {
            return mStatus;
        }

        public void setStatus(final int status) {
            this.mStatus = status;
        }

        public int getDistance() {
            return mDistance;
        }

        public void setDistance(final int distance) {
            this.mDistance = distance;
        }

        public float getPercent() {
            return mPercent;
        }

        public void setPercent(final float percent) {
            this.mPercent = percent;
        }

        public int getTravelTime() {
            return mTravelTime;
        }

        public void setTravelTime(final int travelTime) {
            this.mTravelTime = travelTime;
        }

        @NonNull
        @Override
        public String toString() {
            return "NaviTmcInfoData{" +
                    "number=" + mNumber +
                    ", status=" + mStatus +
                    ", distance=" + mDistance +
                    ", percent=" + mPercent +
                    ", travelTime=" + mTravelTime +
                    '}';
        }
    }

    public NaviLightBarDetail getLightBarDetail() {
        return mLightBarDetail;
    }

    public void setLightBarDetail(final NaviLightBarDetail lightBarDetail) {
        this.mLightBarDetail = lightBarDetail;
    }

    public ArrayList<NaviLightBarInfo> getLightBarInfo() {
        return mLightBarInfo;
    }

    public void setLightBarInfo(final ArrayList<NaviLightBarInfo> lightBarInfo) {
        this.mLightBarInfo = lightBarInfo;
    }

    @NonNull
    @Override
    public String toString() {
        return "NaviTmcInfo{" +
                "lightBarDetail=" + mLightBarDetail +
                ", lightBarInfo=" + mLightBarInfo +
                '}';
    }
}

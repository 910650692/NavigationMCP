package com.sgm.navi.service.define.navi;

import androidx.annotation.NonNull;

import java.util.ArrayList;
import java.util.Arrays;

public class NaviManeuverInfo {

    private int mDateType;

    //转向图标图片数据  ManeuverIconResponseData
    private byte[] mData;//图片数据块, 图片数据块,通知后底层分配者会自动释放,使用者需要copy一份
    private NaviManeuverConfig mRequestConfig;//转向图标请求的参数

    //转向图标信息 ManeuverInfo
    private int mType;//转向图标类型：栅格，矢量
    private long mPathID;//路径id
    private long mSegmentIndex;//所在segment段，从0开始
    private long mManeuverID;//转向动作ID,参见:ManeuverIconID

    //导航过程中传出出入口的编号、方向信息 ExitDirectionInfo
    private ArrayList<String> mExitNameInfo;//出口编号名字数组
    private ArrayList<String> mDirectionInfo;//路牌方向名字数组
    private String mEntranceExit;//出口入口信息 [出口,入口]
    /**
     * 距离当前自车位置距离（单位米）
     */
    private int mDisToCurrentPos;
    /**
     * 预计需要的到达时间（单位秒）
     */
    private int mRemainTime;

    public int getDateType() {
        return mDateType;
    }

    public void setDateType(final int dateType) {
        this.mDateType = dateType;
    }

    public byte[] getData() {
        return mData;
    }

    public void setData(final byte[] data) {
        this.mData = data;
    }

    public NaviManeuverConfig getRequestConfig() {
        return mRequestConfig;
    }

    public void setRequestConfig(final NaviManeuverConfig requestConfig) {
        this.mRequestConfig = requestConfig;
    }

    public int getType() {
        return mType;
    }

    public void setType(final int type) {
        this.mType = type;
    }

    public long getPathID() {
        return mPathID;
    }

    public void setPathID(final long pathID) {
        this.mPathID = pathID;
    }

    public long getSegmentIndex() {
        return mSegmentIndex;
    }

    public void setSegmentIndex(final long segmentIndex) {
        this.mSegmentIndex = segmentIndex;
    }

    public long getManeuverID() {
        return mManeuverID;
    }

    public void setManeuverID(final long maneuverID) {
        this.mManeuverID = maneuverID;
    }

    public ArrayList<String> getExitNameInfo() {
        return mExitNameInfo;
    }

    public void setExitNameInfo(final ArrayList<String> exitNameInfo) {
        this.mExitNameInfo = exitNameInfo;
    }

    public ArrayList<String> getDirectionInfo() {
        return mDirectionInfo;
    }

    public void setDirectionInfo(final ArrayList<String> directionInfo) {
        this.mDirectionInfo = directionInfo;
    }

    public String getEntranceExit() {
        return mEntranceExit;
    }

    public void setEntranceExit(final String entranceExit) {
        this.mEntranceExit = entranceExit;
    }

    public int getDisToCurrentPos() {
        return mDisToCurrentPos;
    }

    public void setDisToCurrentPos(final int disToCurrentPos) {
        this.mDisToCurrentPos = disToCurrentPos;
    }

    public int getRemainTime() {
        return mRemainTime;
    }

    public void setRemainTime(final int remainTime) {
        this.mRemainTime = remainTime;
    }

    @NonNull
    @Override
    public String toString() {
        return "NaviManeuverInfo{" +
                "dateType=" + mDateType +
                ", data=" + Arrays.toString(mData) +
                ", requestConfig=" + mRequestConfig +
                ", type=" + mType +
                ", pathID=" + mPathID +
                ", segmentIndex=" + mSegmentIndex +
                ", maneuverID=" + mManeuverID +
                ", exitNameInfo=" + mExitNameInfo +
                ", directionInfo=" + mDirectionInfo +
                ", entranceExit='" + mEntranceExit + '\'' +
                ", disToCurrentPos=" + mDisToCurrentPos +
                ", remainTime=" + mRemainTime +
                '}';
    }

    public static class NaviManeuverConfig {
        private int mWidth;
        private int mHeight;
        private int mBackColor;
        private int mRoadColor;
        private int mArrowColor;
        private long mPathID;
        private long mSegmentIdx;
        private long mManeuverID;

        public int getWidth() {
            return mWidth;
        }

        public void setWidth(final int width) {
            this.mWidth = width;
        }

        public int getHeight() {
            return mHeight;
        }

        public void setHeight(final int height) {
            this.mHeight = height;
        }

        public int getBackColor() {
            return mBackColor;
        }

        public void setBackColor(final int backColor) {
            this.mBackColor = backColor;
        }

        public int getRoadColor() {
            return mRoadColor;
        }

        public void setRoadColor(final int roadColor) {
            this.mRoadColor = roadColor;
        }

        public int getArrowColor() {
            return mArrowColor;
        }

        public void setArrowColor(final int arrowColor) {
            this.mArrowColor = arrowColor;
        }

        public long getPathID() {
            return mPathID;
        }

        public void setPathID(final long pathID) {
            this.mPathID = pathID;
        }

        public long getSegmentIdx() {
            return mSegmentIdx;
        }

        public void setSegmentIdx(final long segmentIdx) {
            this.mSegmentIdx = segmentIdx;
        }

        public long getManeuverID() {
            return mManeuverID;
        }

        public void setManeuverID(final long maneuverID) {
            this.mManeuverID = maneuverID;
        }

        @NonNull
        @Override
        public String toString() {
            return "NaviManeuverConfig{" +
                    "width=" + mWidth +
                    ", height=" + mHeight +
                    ", backColor=" + mBackColor +
                    ", roadColor=" + mRoadColor +
                    ", arrowColor=" + mArrowColor +
                    ", pathID=" + mPathID +
                    ", segmentIdx=" + mSegmentIdx +
                    ", maneuverID=" + mManeuverID +
                    '}';
        }
    }
}

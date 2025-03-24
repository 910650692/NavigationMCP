package com.fy.navi.service.define.navi;

import java.util.ArrayList;

public class NaviManeuverInfo {

    private int dateType;

    //转向图标图片数据  ManeuverIconResponseData
    private byte[] data;//图片数据块, 图片数据块,通知后底层分配者会自动释放,使用者需要copy一份
    private NaviManeuverConfig requestConfig;//转向图标请求的参数

    //转向图标信息 ManeuverInfo
    private int type;//转向图标类型：栅格，矢量
    private long pathID;//路径id
    private long segmentIndex;//所在segment段，从0开始
    private long maneuverID;//转向动作ID,参见:ManeuverIconID

    //导航过程中传出出入口的编号、方向信息 ExitDirectionInfo
    private ArrayList<String> exitNameInfo;//出口编号名字数组
    private ArrayList<String> directionInfo;//路牌方向名字数组
    private String entranceExit;//出口入口信息 [出口,入口]
    /**
     * 距离当前自车位置距离（单位米）
     */
    public int disToCurrentPos;
    /**
     * 预计需要的到达时间（单位秒）
     */
    public int remainTime;

    public int getDateType() {
        return dateType;
    }

    public void setDateType(int dateType) {
        this.dateType = dateType;
    }

    public byte[] getData() {
        return data;
    }

    public void setData(byte[] data) {
        this.data = data;
    }

    public NaviManeuverConfig getRequestConfig() {
        return requestConfig;
    }

    public void setRequestConfig(NaviManeuverConfig requestConfig) {
        this.requestConfig = requestConfig;
    }

    public int getType() {
        return type;
    }

    public void setType(int type) {
        this.type = type;
    }

    public long getPathID() {
        return pathID;
    }

    public void setPathID(long pathID) {
        this.pathID = pathID;
    }

    public long getSegmentIndex() {
        return segmentIndex;
    }

    public void setSegmentIndex(long segmentIndex) {
        this.segmentIndex = segmentIndex;
    }

    public long getManeuverID() {
        return maneuverID;
    }

    public void setManeuverID(long maneuverID) {
        this.maneuverID = maneuverID;
    }

    public ArrayList<String> getExitNameInfo() {
        return exitNameInfo;
    }

    public void setExitNameInfo(ArrayList<String> exitNameInfo) {
        this.exitNameInfo = exitNameInfo;
    }

    public ArrayList<String> getDirectionInfo() {
        return directionInfo;
    }

    public void setDirectionInfo(ArrayList<String> directionInfo) {
        this.directionInfo = directionInfo;
    }

    public String getEntranceExit() {
        return entranceExit;
    }

    public void setEntranceExit(String entranceExit) {
        this.entranceExit = entranceExit;
    }

    public int getDisToCurrentPos() {
        return disToCurrentPos;
    }

    public void setDisToCurrentPos(int disToCurrentPos) {
        this.disToCurrentPos = disToCurrentPos;
    }

    public int getRemainTime() {
        return remainTime;
    }

    public void setRemainTime(int remainTime) {
        this.remainTime = remainTime;
    }

    public static class NaviManeuverConfig {
        private int width;
        private int height;
        private int backColor;
        private int roadColor;
        private int arrowColor;
        private long pathID;
        private long segmentIdx;
        private long maneuverID;

        public int getWidth() {
            return width;
        }

        public void setWidth(int width) {
            this.width = width;
        }

        public int getHeight() {
            return height;
        }

        public void setHeight(int height) {
            this.height = height;
        }

        public int getBackColor() {
            return backColor;
        }

        public void setBackColor(int backColor) {
            this.backColor = backColor;
        }

        public int getRoadColor() {
            return roadColor;
        }

        public void setRoadColor(int roadColor) {
            this.roadColor = roadColor;
        }

        public int getArrowColor() {
            return arrowColor;
        }

        public void setArrowColor(int arrowColor) {
            this.arrowColor = arrowColor;
        }

        public long getPathID() {
            return pathID;
        }

        public void setPathID(long pathID) {
            this.pathID = pathID;
        }

        public long getSegmentIdx() {
            return segmentIdx;
        }

        public void setSegmentIdx(long segmentIdx) {
            this.segmentIdx = segmentIdx;
        }

        public long getManeuverID() {
            return maneuverID;
        }

        public void setManeuverID(long maneuverID) {
            this.maneuverID = maneuverID;
        }
    }
}

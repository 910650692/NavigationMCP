package com.sgm.navi.fsa.bean;

import java.util.Objects;

/**
 * 图片放大图信息
 */
public class ILSImageViewInfo {
    /**
     * type	int	0 - RGB格式
     */
    private int type;
    /**
     * width	int	放大图宽度
     */
    private int width;
    /**
     * height	int	放大图高度
     */
    private int height;
    /**
     * remainDistance	int	剩余距离，单位：米
     */
    private int remainDistance;
    /**
     * progressRatio	int	进度百分比
     */
    private int progressRatio;
    /**
     * arrowMapName	String	箭头图的名字
     */
    private String arrowMapName;
    /**
     * backgroundMapName	String	背景图的名字
     */
    private String backgroundMapName;
    /**
     * nextRoadName	String	放大图所在路名
     */
    private String nextRoadName;
    /**
     * backgroundMapBytes	String	背景图，Base64字符串png/jpg
     */
    private String backgroundMapBytes;
    /**
     * arrowMapBytes	String	箭头图，Base64字符串png/jpg
     */
    private String arrowMapBytes;
    /**
     * backgroundMimeType	String	背景图类型。“image/png”或“image/jpeg”
     */
    private String backgroundMimeType;
    /**
     * arrowMimeType	String	箭头图类型。“image/png”或“image/jpeg”
     */
    private String arrowMimeType;

    public int getType() {
        return type;
    }

    public void setType(int type) {
        this.type = type;
    }

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

    public int getRemainDistance() {
        return remainDistance;
    }

    public void setRemainDistance(int remainDistance) {
        this.remainDistance = remainDistance;
    }

    public int getProgressRatio() {
        return progressRatio;
    }

    public void setProgressRatio(int progressRatio) {
        this.progressRatio = progressRatio;
    }

    public String getArrowMapName() {
        return arrowMapName;
    }

    public void setArrowMapName(String arrowMapName) {
        this.arrowMapName = arrowMapName;
    }

    public String getBackgroundMapName() {
        return backgroundMapName;
    }

    public void setBackgroundMapName(String backgroundMapName) {
        this.backgroundMapName = backgroundMapName;
    }

    public String getNextRoadName() {
        return nextRoadName;
    }

    public void setNextRoadName(String nextRoadName) {
        this.nextRoadName = nextRoadName;
    }

    public String getBackgroundMapBytes() {
        return backgroundMapBytes;
    }

    public void setBackgroundMapBytes(String backgroundMapBytes) {
        this.backgroundMapBytes = backgroundMapBytes;
    }

    public String getArrowMapBytes() {
        return arrowMapBytes;
    }

    public void setArrowMapBytes(String arrowMapBytes) {
        this.arrowMapBytes = arrowMapBytes;
    }

    public String getBackgroundMimeType() {
        return backgroundMimeType;
    }

    public void setBackgroundMimeType(String backgroundMimeType) {
        this.backgroundMimeType = backgroundMimeType;
    }

    public String getArrowMimeType() {
        return arrowMimeType;
    }

    public void setArrowMimeType(String arrowMimeType) {
        this.arrowMimeType = arrowMimeType;
    }

    @Override
    public String toString() {
        return "ILSImageViewInfo{" +
                "type=" + type +
                ", width=" + width +
                ", height=" + height +
                ", remainDistance=" + remainDistance +
                ", progressRatio=" + progressRatio +
                ", arrowMapName='" + arrowMapName + '\'' +
                ", backgroundMapName='" + backgroundMapName + '\'' +
                ", nextRoadName='" + nextRoadName + '\'' +
                ", backgroundMapBytes='" + (Objects.isNull(backgroundMapBytes) ? "null" : backgroundMapBytes.length()) + '\'' +
                ", arrowMapBytes='" + (Objects.isNull(arrowMapBytes) ? "null" : arrowMapBytes.length()) + '\'' +
                ", backgroundMimeType='" + backgroundMimeType + '\'' +
                ", arrowMimeType='" + arrowMimeType + '\'' +
                '}';
    }
}

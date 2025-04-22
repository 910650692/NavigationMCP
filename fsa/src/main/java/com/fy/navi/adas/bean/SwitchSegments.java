package com.fy.navi.adas.bean;

import com.google.gson.annotations.SerializedName;

/**
 * 基于SD的道路matching类型分段
 *
 * 一条路径线由N个段组成，每个段由N个链路组成，每个链路由N个点组成
 */
public class SwitchSegments {
    /**
     * segment起点在SD上对应坐标点序列的index
     */
    private int begin;
    /**
     * segment起点在SD上对应link的index
     */
    private int begin_link_index;
    /**
     * segment起点在SD上对应link的offset
     */
    private double begin_link_offset;
    /**
     * 坐标点序列
     */
    private Coord[] coords;
    /**
     * segment终点在SD上对应坐标点序列的index
     */
    private int end;
    /**
     * segment终点在SD上对应link的index
     */
    private int end_link_index;
    /**
     * segment终点在SD上对应link的offset
     */
    private double end_link_offset;
    private double length;
    /**
     * segment对应的智驾功能类型
     * •0: unmatch
     * •9, sd unp
     * •10, sd hnp
     * •11, sd odd close
     */
    private int mode;
    /**
     * 类型描述
     */
    private String mode_describe;
    /**
     * 区间的长度，单位米
     */
    @SerializedName("length")
    private int mLength;

    public int getmLength() {
        return mLength;
    }

    public void setmLength(int mLength) {
        this.mLength = mLength;
    }

    public String getMode_describe() {
        return mode_describe;
    }

    public void setMode_describe(String mode_describe) {
        this.mode_describe = mode_describe;
    }

    public int getMode() {
        return mode;
    }

    public void setMode(int mode) {
        this.mode = mode;
    }

    public double getLength() {
        return length;
    }

    public void setLength(double length) {
        this.length = length;
    }

    public double getEnd_link_offset() {
        return end_link_offset;
    }

    public void setEnd_link_offset(double end_link_offset) {
        this.end_link_offset = end_link_offset;
    }

    public int getEnd_link_index() {
        return end_link_index;
    }

    public void setEnd_link_index(int end_link_index) {
        this.end_link_index = end_link_index;
    }

    public int getEnd() {
        return end;
    }

    public void setEnd(int end) {
        this.end = end;
    }

    public Coord[] getCoords() {
        return coords;
    }

    public void setCoords(Coord[] coords) {
        this.coords = coords;
    }

    public double getBegin_link_offset() {
        return begin_link_offset;
    }

    public void setBegin_link_offset(double begin_link_offset) {
        this.begin_link_offset = begin_link_offset;
    }

    public int getBegin_link_index() {
        return begin_link_index;
    }

    public void setBegin_link_index(int begin_link_index) {
        this.begin_link_index = begin_link_index;
    }

    public int getBegin() {
        return begin;
    }

    public void setBegin(int begin) {
        this.begin = begin;
    }
}

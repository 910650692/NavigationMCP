package com.fy.navi.adas.bean;

import com.google.gson.annotations.SerializedName;

public class SwitchSegments {
    private int begin;
    private int begin_link_index;
    private double begin_link_offset;
    private Coord[] coords;
    private int end;
    private int end_link_index;
    private double end_link_offset;
    private double length;
    private int mode;
    private String mode_describe;
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

package com.sgm.navi.adas.bean;

import java.util.ArrayList;

class GuideGroup {
    private int groupIconType;
    private int groupLen;
    private int groupTrafficLightsCount;
    private ArrayList<Segment> segments;

    public int getGroupIconType() {
        return groupIconType;
    }

    public void setGroupIconType(int groupIconType) {
        this.groupIconType = groupIconType;
    }

    public int getGroupLen() {
        return groupLen;
    }

    public void setGroupLen(int groupLen) {
        this.groupLen = groupLen;
    }

    public int getGroupTrafficLightsCount() {
        return groupTrafficLightsCount;
    }

    public void setGroupTrafficLightsCount(int groupTrafficLightsCount) {
        this.groupTrafficLightsCount = groupTrafficLightsCount;
    }

    public ArrayList<Segment> getSegments() {
        return segments;
    }

    public void setSegments(ArrayList<Segment> segments) {
        this.segments = segments;
    }
}

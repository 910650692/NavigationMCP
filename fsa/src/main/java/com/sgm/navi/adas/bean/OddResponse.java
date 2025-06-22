package com.sgm.navi.adas.bean;

import java.util.ArrayList;

class OddResponse {
    private EndPoi endPoi;
    private String engineVer;
    private ArrayList<GuideGroup> guideGroups;
    private InterfaceMeta interfaceMeta;
    private int linkCnt;
    private ArrayList<Link> links;
    private long path_id;
    private int pntCnt;
    private ArrayList<Pnt> pnts;
    private String sdkVer;
    private ArrayList<SwitchSegment> switch_segments;
    private ArrayList<TrafficLight> trafficLights;

    public EndPoi getEndPoi() {
        return endPoi;
    }

    public void setEndPoi(EndPoi endPoi) {
        this.endPoi = endPoi;
    }

    public String getEngineVer() {
        return engineVer;
    }

    public void setEngineVer(String engineVer) {
        this.engineVer = engineVer;
    }

    public ArrayList<GuideGroup> getGuideGroups() {
        return guideGroups;
    }

    public void setGuideGroups(ArrayList<GuideGroup> guideGroups) {
        this.guideGroups = guideGroups;
    }

    public InterfaceMeta getInterfaceMeta() {
        return interfaceMeta;
    }

    public void setInterfaceMeta(InterfaceMeta interfaceMeta) {
        this.interfaceMeta = interfaceMeta;
    }

    public int getLinkCnt() {
        return linkCnt;
    }

    public void setLinkCnt(int linkCnt) {
        this.linkCnt = linkCnt;
    }

    public ArrayList<Link> getLinks() {
        return links;
    }

    public void setLinks(ArrayList<Link> links) {
        this.links = links;
    }

    public long getPath_id() {
        return path_id;
    }

    public void setPath_id(long path_id) {
        this.path_id = path_id;
    }

    public int getPntCnt() {
        return pntCnt;
    }

    public void setPntCnt(int pntCnt) {
        this.pntCnt = pntCnt;
    }

    public ArrayList<Pnt> getPnts() {
        return pnts;
    }

    public void setPnts(ArrayList<Pnt> pnts) {
        this.pnts = pnts;
    }

    public String getSdkVer() {
        return sdkVer;
    }

    public void setSdkVer(String sdkVer) {
        this.sdkVer = sdkVer;
    }

    public ArrayList<SwitchSegment> getSwitch_segments() {
        return switch_segments;
    }

    public void setSwitch_segments(ArrayList<SwitchSegment> switch_segments) {
        this.switch_segments = switch_segments;
    }

    public ArrayList<TrafficLight> getTrafficLights() {
        return trafficLights;
    }

    public void setTrafficLights(ArrayList<TrafficLight> trafficLights) {
        this.trafficLights = trafficLights;
    }
}

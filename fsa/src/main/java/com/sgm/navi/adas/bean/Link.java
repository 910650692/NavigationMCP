package com.sgm.navi.adas.bean;

class Link {
    private int adminCode;
    private int formway;
    private int len;
    private int linkID;
    private int linktype;
    private int pntBegIdx;
    private int pntCnt;
    private int roadclass;
    private String roadname;

    public int getAdminCode() {
        return adminCode;
    }

    public void setAdminCode(int adminCode) {
        this.adminCode = adminCode;
    }

    public int getFormway() {
        return formway;
    }

    public void setFormway(int formway) {
        this.formway = formway;
    }

    public int getLen() {
        return len;
    }

    public void setLen(int len) {
        this.len = len;
    }

    public int getLinkID() {
        return linkID;
    }

    public void setLinkID(int linkID) {
        this.linkID = linkID;
    }

    public int getLinktype() {
        return linktype;
    }

    public void setLinktype(int linktype) {
        this.linktype = linktype;
    }

    public int getPntBegIdx() {
        return pntBegIdx;
    }

    public void setPntBegIdx(int pntBegIdx) {
        this.pntBegIdx = pntBegIdx;
    }

    public int getPntCnt() {
        return pntCnt;
    }

    public void setPntCnt(int pntCnt) {
        this.pntCnt = pntCnt;
    }

    public int getRoadclass() {
        return roadclass;
    }

    public void setRoadclass(int roadclass) {
        this.roadclass = roadclass;
    }

    public String getRoadname() {
        return roadname;
    }

    public void setRoadname(String roadname) {
        this.roadname = roadname;
    }
}

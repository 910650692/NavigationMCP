package com.fy.navi.fsa.bean;

public class EnlargeMap {
    private int type;
    private int status;
    private ILSImageViewInfo ilsImageViewInfo;
    private JunctionViewInfo junctionViewInfo;

    public EnlargeMap() {
    }

    public EnlargeMap(int type, int status, ILSImageViewInfo ilsImageViewInfo, JunctionViewInfo junctionViewInfo) {
        this.type = type;
        this.status = status;
        this.ilsImageViewInfo = ilsImageViewInfo;
        this.junctionViewInfo = junctionViewInfo;
    }

    public int getType() {
        return type;
    }

    public void setType(int type) {
        this.type = type;
    }

    public int getStatus() {
        return status;
    }

    public void setStatus(int status) {
        this.status = status;
    }

    public ILSImageViewInfo getIlsImageViewInfo() {
        return ilsImageViewInfo;
    }

    public void setIlsImageViewInfo(ILSImageViewInfo ilsImageViewInfo) {
        this.ilsImageViewInfo = ilsImageViewInfo;
    }

    public JunctionViewInfo getJunctionViewInfo() {
        return junctionViewInfo;
    }

    public void setJunctionViewInfo(JunctionViewInfo junctionViewInfo) {
        this.junctionViewInfo = junctionViewInfo;
    }

    @Override
    public String toString() {
        return "EnlargeMap{" +
                "type=" + type +
                ", status=" + status +
                ", ilsImageViewInfo=" + ilsImageViewInfo +
                ", junctionViewInfo=" + junctionViewInfo +
                '}';
    }
}

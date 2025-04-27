package com.fy.navi.service.adapter.search.cloudByPatac.req;

public class StationReq extends BaseReq{
    private String mAreaCode;

    public StationReq(String apiVersion) {
        super(apiVersion);
    }

    public StationReq(String apiVersion, String idpUserId) {
        super(apiVersion, idpUserId);
    }

    public String getmAreaCode() {
        return mAreaCode;
    }

    public void setmAreaCode(String mAreaCode) {
        this.mAreaCode = mAreaCode;
    }
}

package com.fy.navi.vrbridge.bean;

import com.baidu.oneos.protocol.callback.PoiCallback;
import com.baidu.oneos.protocol.callback.RespCallback;

//单个语音指令
public class SingleCommandInfo {

    private int mIntParam;
    private String mPoiName;
    private String mPoiType;
    private PoiCallback mPoiCallback;
    private RespCallback mRespCallback;

    public int getIntParam() {
        return mIntParam;
    }

    public void setIntParam(final int intParam) {
        mIntParam = intParam;
    }

    public String getPoiName() {
        return mPoiName;
    }

    public void setPoiName(final String poiName) {
        mPoiName = poiName;
    }

    public String getPoiType() {
        return mPoiType;
    }

    public void setPoiType(final String poiType) {
        mPoiType = poiType;
    }

    public PoiCallback getPoiCallback() {
        return mPoiCallback;
    }

    public void setPoiCallback(final PoiCallback poiCallback) {
        mPoiCallback = poiCallback;
    }

    public RespCallback getRespCallback() {
        return mRespCallback;
    }

    public void setRespCallback(final RespCallback respCallback) {
        mRespCallback = respCallback;
    }

}
